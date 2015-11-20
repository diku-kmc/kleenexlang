{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module KMC.Util.Bits ( DoubleBits(combine, split), packCombine, unpackSplit ) where

import Data.Bits (shift, (.|.), (.&.), complement, bitSize, clearBit, Bits(bit))
import Data.Word (Word8, Word16, Word32, Word64)

-- | Pack two 8-bit words in a 16-bit word, two 16-bit words in a 32-bit word,
-- etc.  Split a 16-bit word into two 8-bit words, etc.
class (Bits b, Enum b, Bits (Twice b), Enum (Twice b)) => DoubleBits b where
    type Twice b :: *
    combine :: (b, b) -> Twice b
    combine (w1, w2) = (castWord w1 `shift` (bitSize w1)) .|. (castWord w2)

    -- The actual split implementation requires a zero element of the correct
    -- type, so the type checker knows which instance to pick.
    split' :: b -> Twice b -> (b, b)
    split' z w = ( castWord $ (w .&. hi) `shift` (negate (bitSize w `div` 2))
                 , castWord $ w .&. lo
                 )
        where
          hi = combine (complement z, z)
          lo = combine (z, complement z)

    split :: (Twice b) -> (b, b)
    -- The implementation is just split' but with a type-annotated zero element
    -- given to make the type checker happy.  It complains about Twice being
    -- non-injective, so we help it with type annotations in the instances.

zeroBits :: (Bits a) => a
zeroBits = clearBit (bit 0) 0

instance DoubleBits Word8 where
    type Twice Word8 = Word16
    split = split' (zeroBits :: Word8)

instance DoubleBits Word16 where
    type Twice Word16 = Word32
    split = split' (zeroBits :: Word16)

instance DoubleBits Word32 where
    type Twice Word32 = Word64
    split = split' (zeroBits :: Word32)
             
packCombine :: (DoubleBits b) => b -> [b] -> [Twice b]
packCombine _ []           = []
packCombine end (w1:w2:ws) = combine (w1, w2) : packCombine end ws
packCombine end [w1]       = [combine (w1, end)]

unpackSplit :: (DoubleBits b) => [Twice b] -> [b]
unpackSplit = concatMap ((\(x,y) -> [x,y]) . split)

word8toWord16 :: (Word8, Word8) -> Word16
word8toWord16 = combine
word16toWord8 :: Word16 -> (Word8, Word8)
word16toWord8 = split

castWord :: (Enum a, Enum b) => a -> b
castWord = toEnum . fromEnum
