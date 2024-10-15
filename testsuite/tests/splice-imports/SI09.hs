{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import splice InstanceA ()
import splice ClassA
import splice Prelude
-- Why is implicit prelude import broken?
import Prelude

e :: IO ()
-- Uses a non-splice imported instance
e = $(const [| pure () |] (x vx))
