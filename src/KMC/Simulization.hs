{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module KMC.Simulization where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Word
import           Text.Printf

import           KMC.SymbolicFST
import           KMC.SymbolicFST.Transducer
import           KMC.Kleenex.Actions
import           KMC.RangeSet


data Out =
  Sym [Word8]
  | Reg RegAction
  | Empty
  deriving (Show)


data TState=
  Choice Int Int
  | Skip Int Out
  | Symbol Int [(Word8, Word8)] (Maybe Out) -- The Word8 in maybe not currently used
  | Accept
  deriving (Show)

type IState = (Int, TState)

type NFST = (Int, [IState])

type Tr = Transducer Int Word8 (Either Word8 RegAction)

test :: [Either a b] -> [a]
test [] = []
test (l : ll) = case l of
                Left w  -> w : test ll
                Right _ -> error "No"

convState :: Tr -> Int -> IState
convState fst' k = let i = fromEnum k in
  case M.lookup k (eForward (fstE fst')) of
    Just ((p, CopyArg, t) : _) -> (i, Symbol t (ranges p) Nothing)
    Just ((p, CopyConst (Left w : _), t) : _) -> (i, Symbol t (ranges p) (Just (Sym [w])))
    Just ((p, CopyConst (Right r : _), t) : _) -> (i, Symbol t (ranges p) (Just (Reg r)))
    Just ((p, CopyConst [], t) : _) -> (i, Symbol t (ranges p) (Just Empty))
    Just []              -> error "No edges for state"
    Nothing -> case M.lookup k (eForwardEpsilon (fstE fst')) of
      Just [(Right r : _, t)] -> (i, Skip t (Reg r))
      Just [([], t)]            -> (i, Skip t Empty)
      Just [(w, t)]  -> (i, Skip t (Sym (test w)))
      Just ((_, t1) : [(_, t2)])   -> (i, Choice t1 t2)
      Just []                        -> error "No epsilon edges for state"
      Nothing                        -> (i, Accept) -- Should be guranteed


fstToNFST :: Tr -> NFST
fstToNFST fst' =
  let eFST = enumerateStates fst'
      sts  = map (convState eFST) (S.toList (fstS eFST))
  in (fstI eFST, sts)


stToString :: IState -> String
stToString (i, st) =
  case st of
    Choice t1 t2 -> printf "%i C %i %i\n" i t1 t2
    Skip t a       -> let pe = printf "%i S %i " i t in
                        case a of
                            (Sym w) -> pe ++ printf "W %i %s\n" (length w) (show w)
                            (Reg r) -> pe ++ printf "R %s\n" (show r)
                            Empty   -> pe ++ "E\n"
    Symbol t p a -> let pe = printf "%i R %i %i %s " i t (length p) (show p) in
                        case a of
                            Nothing -> pe ++ "C\n"
                            (Just (Sym w)) -> pe ++ printf "W %i %s\n" (length w) (show w)
                            (Just (Reg r)) -> pe ++ printf "R %s\n" (show r)
                            (Just Empty)   -> pe ++ "E\n"
    Accept       -> printf "%i A\n" i

nfstToCsv :: NFST -> String
nfstToCsv (i, nfst') = let
  line1 = printf "%i %i\n" (length nfst') i
  sts   = map stToString nfst'
  in line1 ++ concat sts
