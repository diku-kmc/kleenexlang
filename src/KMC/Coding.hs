{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module KMC.Coding
    (bitWidth
    ,codeFixedWidth
    ,codeFixedWidthEnum
    ,codeFixedWidthEnumSized
    ,decode
    ,decodeEnum)
where

-- | Compute the bit width to fit n values in a word of digits of a given base
bitWidth :: Int -> Int -> Int
bitWidth base n = go 0
    where
      go w | base^w >= n = w
           | otherwise   = go (w+1)
  
-- | Code an integral value as a big-endian sequence of digits in an arbitrary base.
codeFixedWidth :: Int -> Int -> Int -> [Int]
codeFixedWidth base width ndata = go width ndata
    where
      go 0 _  = []
      go w nd = let exp' = base^(w-1)
                    (q, r) = divMod nd exp'
                in if exp' <= 0 then
                       0 : go (w-1) nd
                   else
                       if q >= base then
                           error $ "Integer out of bounds: Cannot code "
                                     ++ show ndata ++ " in "
                                     ++ show width ++ " decimals, base "
                                     ++ show base
                       else
                           q:go (w-1) r

codeFixedWidthEnum :: forall b. (Enum b, Bounded b) => Int -> Int -> [b]
codeFixedWidthEnum width ndata = map toEnum $ codeFixedWidth base width ndata
    where
      base = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1

codeFixedWidthEnumSized :: forall b. (Enum b, Bounded b) => Int -> Int -> [b]
codeFixedWidthEnumSized size ndata = map toEnum $ codeFixedWidth base width ndata
    where
      base = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1
      width = bitWidth base size

decode :: Int -> [Int] -> Int
decode base ds = go (length ds) ds
    where
      go _ [] = 0
      go n (d:ds') = base^(n-1) * d + go (n-1) ds'

decodeEnum :: forall b. (Enum b, Bounded b) => [b] -> Int
decodeEnum ds = decode base (map fromEnum ds)
    where
      base = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1
