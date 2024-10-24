{-# LANGUAGE NoPathCSP #-}
{-# LANGUAGE DeriveLift #-}
module SI15 where

import Language.Haskell.TH.Syntax (Lift)

data A = A deriving Lift
