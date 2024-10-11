{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI02 where

import splice Prelude

main :: IO ()
main = $(id [| pure () |])
