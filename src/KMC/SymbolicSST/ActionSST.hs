{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
module KMC.SymbolicSST.ActionSST(ActionSST, actionToSST) where

import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           KMC.Kleenex.Actions (RegAction(..))
import           KMC.Kleenex.Syntax (RegIdent(..))
import           KMC.RangeSet (RangeSet)
import           KMC.SymbolicFST (FST(..))
import qualified KMC.SymbolicFST as FST
import           KMC.SymbolicFST.ActionMachine (ActionMachine, CodeInputLab(..), DecodeFunc(..))
import           KMC.SymbolicSST

{-
Action machines are deterministic by construction, and thus may be seen as a
deterministic SST with only one register. Register effects adjoined to the
output alphabet can also be interpreted as SST actions.
-}

type ActionSST st digit sigma var
  = SST st (CodeInputLab digit) (DecodeFunc (RangeSet sigma) digit (Identity sigma)) var

actionToSST :: forall st sigma digit.
               (Ord st, Ord sigma, Enum sigma, Bounded sigma, Enum digit, Bounded digit)
               => ActionMachine st sigma RegAction digit -> ActionSST (st, Int) digit sigma Int
actionToSST actM = construct' (fstI actM, 0) edges outs
  where
    regs = M.fromList [ (r, maxBound - i) | r <- S.toList (registers actM) | i <- [0..] ]
           :: M.Map RegIdent Int
    (edges, outs) = go (S.singleton (fstI actM, 0)) S.empty [] []

    go ws states es os
      | S.null ws = (es, os)
      | (s, ws') <- S.deleteFindMin ws, S.member s states = go ws' states es os
      | (s@(q,h),ws') <- S.deleteFindMin ws =
          let (y, q') = followEps actM q
              (h', kappa) = interp regs h y
                :: (Int, RegisterUpdate Int (DecodeFunc (RangeSet sigma) digit (Identity sigma)))
              os' = if S.member q' (fstF actM) then
                      [(s, evalUpdateStringFunc [error "input function emitted along epsilon path"]
                           $ fromMaybe [VarA 0] (M.lookup 0 kappa))]
                    else []
              es' = [ (s,ps,ru,s') | (ps, ru, s') <- next actM regs (q',h') ]
              ws'' = S.fromList [ s' | (_, _, _, s') <- es' ]
              states' = S.insert s states
          in go (S.union ws'' ws') states' (es' ++ es) (os' ++ os)

-- | Extract all occurrences of register identifiers in an action machine
registers :: ActionMachine st sigma RegAction digit -> S.Set RegIdent
registers actM = S.unions [ ext l | (_, l, _) <- FST.edgesToList $ fstE actM ]
  where
    ext (Left (_, DecodeArg _)) = S.empty
    ext (Left (_, DecodeConst xs)) = S.fromList $ concatMap extAct [ act | Right act <- xs ]
    ext (Right xs) = S.fromList $ concatMap extAct [ act | Right act <- xs ]

    extAct Push = []
    extAct (Pop r) = [r]
    extAct (Write r) = [r]

-- | Interpret a sequence of action symbols as a register update
interp :: M.Map RegIdent Int -> Int -> [Either sigma RegAction]
       -> (Int, RegisterUpdate Int (DecodeFunc (RangeSet sigma) digit (Identity sigma)))
interp regs = go
  where
    go h [] = (h, M.empty)
    go h (Left a:xs) =
      let (h', kappa) = go h xs
          kappa' = M.singleton h [VarA h, ConstA [a]]
      in (h', composeRegisterUpdate kappa' kappa)
    go h (Right Push:xs) =
      let (h', kappa) = go (h+1) xs
      in (h', kappa)
    go h (Right (Pop r):xs) =
      let (h', kappa) = go (h-1) xs
          kappa' = M.fromList [(h, []), (regs M.! r, [VarA h])]
      in (h', composeRegisterUpdate kappa' kappa)
    go h (Right (Write r):xs) =
      let (h', kappa) = go h xs
          kappa' = M.fromList [(regs M.! r, []), (h, [VarA h, VarA $ regs M.! r])]
      in (h', composeRegisterUpdate kappa' kappa)

followEps :: (Ord st) => ActionMachine st sigma act digit -> st -> ([Either sigma act], st)
followEps actM q =
  case FST.fstEvalEpsilonEdges actM q of
  []          -> ([], q)
  [(out, q')] -> let (out', q'') = followEps actM q' in (out ++ out', q'')
  _           -> error "non-deterministic action machine"

next :: (Ord st)
     => ActionMachine st sigma RegAction digit
     -> M.Map RegIdent Int
     -> (st, Int)
     -> [([CodeInputLab digit]
         ,RegisterUpdate Int (DecodeFunc (RangeSet sigma) digit (Identity sigma))
         ,(st, Int))]
next actM regs (q, h) = do
  (p, f, q'') <- fromMaybe [] (M.lookup q (FST.eForward . fstE $ actM))
  let (us, q')     = followEps actM q''
  let (h'', kappa) = case f of
                     DecodeConst c -> interp regs h c
                     DecodeArg es  -> (h, M.singleton h [VarA h, FuncA (DecodeArg es) 0])
  let (h', kappa') = interp regs h'' us
  return ([p], composeRegisterUpdate kappa kappa', (q', h'))
