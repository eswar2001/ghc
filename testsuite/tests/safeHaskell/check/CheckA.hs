{-# LANGUAGE Trustworthy #-}
module CheckA (
        trace
    ) where

import qualified Debug.Trace as D
import qualified Data.ByteString.Lazy.Char8 as BS

-- | Allowed declassification
trace :: String -> a -> a
trace s = D.trace $ s ++ show a3

a3 :: BS.ByteString
a3 = BS.take 3 $ BS.repeat 'a'

