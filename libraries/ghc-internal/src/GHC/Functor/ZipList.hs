{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Functor.ZipList (ZipList(..)) where

import GHC.Base
import GHC.Generics
import GHC.List (repeat, zipWith)
import GHC.Read (Read)
import GHC.Show (Show)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable(..))
import Data.Data (Data)

-- | Lists, but with an 'Applicative' functor based on zipping.
--
-- ==== __Examples__
--
-- In contrast to the 'Applicative' for 'GHC.List.List':
--
-- >>> (+) <$> [1, 2, 3] <*> [4, 5, 6]
-- [5,6,7,6,7,8,7,8,9]
--
-- The Applicative instance of ZipList applies the operation
-- by pairing up the elements, analogous to 'zipWith'N
--
-- >>> (+) <$> ZipList [1, 2, 3] <*> ZipList [4, 5, 6]
-- ZipList {getZipList = [5,7,9]}
--
-- >>> (,,,) <$> ZipList [1, 2] <*> ZipList [3, 4] <*> ZipList [5, 6] <*> ZipList [7, 8]
-- ZipList {getZipList = [(1,3,5,7),(2,4,6,8)]}
--
-- >>> ZipList [(+1), (^2), (/ 2)] <*> ZipList [5, 5, 5]
-- ZipList {getZipList = [6.0,25.0,2.5]}
newtype ZipList a = ZipList { getZipList :: [a] }
                  deriving ( Show     -- ^ @since 4.7.0.0
                           , Eq       -- ^ @since 4.7.0.0
                           , Ord      -- ^ @since 4.7.0.0
                           , Read     -- ^ @since 4.7.0.0
                           , Functor  -- ^ @since 2.01
                           , Foldable -- ^ @since 4.9.0.0
                           , Generic  -- ^ @since 4.7.0.0
                           , Generic1 -- ^ @since 4.7.0.0
                           )
-- See Data.Traversable for Traversable instance due to import loops


-- | @since 4.9.0.0
instance Traversable ZipList where
    traverse f (ZipList x) = ZipList `fmap` traverse f x

-- |
-- > f <$> ZipList xs1 <*> ... <*> ZipList xsN
-- >     = ZipList (zipWithN f xs1 ... xsN)
--
-- where @zipWithN@ refers to the @zipWith@ function of the appropriate arity
-- (@zipWith@, @zipWith3@, @zipWith4@, ...). For example:
--
-- > (\a b c -> stimes c [a, b]) <$> ZipList "abcd" <*> ZipList "567" <*> ZipList [1..]
-- >     = ZipList (zipWith3 (\a b c -> stimes c [a, b]) "abcd" "567" [1..])
-- >     = ZipList {getZipList = ["a5","b6b6","c7c7c7"]}
--
-- @since 2.01
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

-- | @since 4.11.0.0
instance Alternative ZipList where
   empty = ZipList []
   ZipList xs0 <|> ZipList ys0 = ZipList $ go xs0 ys0
     where
       go (x:xs) (_:ys) = x : go xs ys
       go    []     ys  = ys
       go    xs      _  = xs

-- | @since 4.14.0.0
deriving instance Data a => Data (ZipList a)

