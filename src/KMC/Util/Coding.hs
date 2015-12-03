{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module KMC.Util.Coding
    (bitWidth
    ,boundedSize
    ,codeFixedWidth
    ,codeFixedWidthEnum
    ,codeFixedWidthEnumSized
    ,decode
    ,decodeEnum)
where

bitWidth :: (Integral b, Num a, Ord a)
         => a -- ^ Base
         -> a -- ^ Domain size
         -> b -- ^ Digit count
bitWidth base n = go 0
    where
      go w | base^w >= n = w
           | otherwise   = go (w+1)

-- | Get the size of a bounded domain.
boundedSize :: forall b s. (Bounded b, Enum b, Num s)
               => b   -- ^ Dummy argument. Not evaluated
               -> s -- ^ Size of domain
boundedSize _ = fromInteger $ toInteger (fromEnum (maxBound :: b))
                            - toInteger (fromEnum (minBound :: b)) + 1

-- | Code an integral value as a big-endian sequence of digits in an arbitrary base.
codeFixedWidth :: Int   -- ^ Base
               -> Int   -- ^ Number of digits
               -> Int   -- ^ Value
               -> [Int]
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

-- | Code a value in a fixed number of digits of an arbitrary base
codeFixedWidthEnum :: forall b. (Enum b, Bounded b) =>
                      Int -- ^ Number of digits to code the value in
                   -> Int -- ^ Value
                   -> [b]
codeFixedWidthEnum width ndata = map toEnum $ codeFixedWidth base width ndata
    where
      base = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1

-- | Code a value in a fixed number of digits of an arbitrary base. The number
-- of digits required is inferred from the size of the domain.
codeFixedWidthEnumSized :: forall b. (Enum b, Bounded b) =>
                           Int -- ^ Size of domain
                        -> Int -- ^ Value
                        -> [b]
codeFixedWidthEnumSized size' ndata = map toEnum $ codeFixedWidth base width ndata
    where
      base = fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1
      width = bitWidth base size'

-- | Decode a value encoded as digits in a given base
decode :: (Num a) => a   -- ^ Base
       -> [a]            -- ^ Digits
       -> a
decode base ds = go (length ds) ds
    where
      go _ [] = 0
      go n (d:ds') = base^(n-1) * d + go (n-1) ds'

-- | Decode a value encoded as digits in an enumerable bounded base.
decodeEnum :: forall a b. (Enum b, Bounded b, Enum a, Num a) => [b] -> a
decodeEnum ds = decode base (map (toEnum . fromEnum) ds)
    where
      base = toEnum (fromEnum (maxBound :: b) - fromEnum (minBound :: b) + 1)
