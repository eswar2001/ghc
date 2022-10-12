-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- {-# OPTIONS_GHC -O2 -fforce-recomp #-}
-- {-# LANGUAGE PatternSynonyms #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE MagicHash, UnboxedTuples #-}

module T14844Example (topLvl) where

topLvl large = (bar1, bar2, foo)
  where
    foo :: Integer -> (a -> b -> Bool) -> (a,b) -> Bool
    foo 0 _ _ = False
    foo s f t = l s' t
       where
         l 0 t = False
         l 1 t = case t of (x,y) -> f x y
         l n (x,y) = l (n-1) (x,y)
         s' = large s

    bar1 :: Integer -> (a -> b -> Bool) -> a -> b -> Bool
    bar1 s f x y = foo s f (x,y)

    bar2 :: Integer ->  (a -> b -> Bool) -> a -> b -> Bool
    bar2 s f x y = foo (s + 1) f (x,y)
