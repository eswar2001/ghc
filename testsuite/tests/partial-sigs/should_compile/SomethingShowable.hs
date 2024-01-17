{-# LANGUAGE FlexibleContexts, PartialTypeSignatures, NamedWildCards, NoMonoLocalBinds #-}
module SomethingShowable where

somethingShowable :: Show _x => _x -> _
somethingShowable x = show (not x)
-- Inferred type: Bool -> String
