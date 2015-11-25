module KMC.Util.UTF8 (encodeChar) where

import           Data.ByteString (unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word

encodeChar :: Char -> [Word8]
encodeChar = unpack . TE.encodeUtf8 . T.singleton
