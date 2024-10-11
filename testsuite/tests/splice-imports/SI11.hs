{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI11 where

-- Is path-based CSP banned?
data X = X

x X = [| X |]




