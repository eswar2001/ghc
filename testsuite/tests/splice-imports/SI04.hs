{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI04 where

import SI01A
import splice SI01A

main :: IO ()
main = $( sid [| pure () |]) >> $$( sid [|| pure () ||])
