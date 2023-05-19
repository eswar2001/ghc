{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base.Semigroup
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- 'Monoid' and 'Semigroup' classes.
--
-----------------------------------------------------------------------------

module GHC.Base.Semigroup
    ( Semigroup(..)
    , Monoid(..)
    ) where

import GHC.Types
import GHC.Maybe
import GHC.Base.List (foldr, map, (++))
import GHC.Base.NonEmpty
import GHC.Tuple (Solo (MkSolo)) -- Note [Depend on GHC.Tuple]
import {-# SOURCE #-} GHC.Real (Integral)
import {-# SOURCE #-} Data.Semigroup.Internal ( stimesDefault
                                              , stimesMaybe
                                              , stimesList
                                              , stimesIdempotentMonoid
                                              )

infixr 6 <>

-- | The class of semigroups (types with an associative binary operation).
--
-- Instances should satisfy the following:
--
-- [Associativity] @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
--
-- You can alternatively define `sconcat` instead of (`<>`), in which case the
-- laws are:
--
-- [Unit]: @'sconcat' ('pure' x) = x@
-- [Multiplication]: @'sconcat' ('join' xss) = 'sconcat' ('fmap' 'sconcat' xss)@
--
-- @since 4.9.0.0
class Semigroup a where
        -- | An associative operation.
        --
        -- >>> [1,2,3] <> [4,5,6]
        -- [1,2,3,4,5,6]
        (<>) :: a -> a -> a
        a <> b = sconcat (a :| [ b ])

        -- | Reduce a non-empty list with '<>'
        --
        -- The default definition should be sufficient, but this can be
        -- overridden for efficiency.
        --
        -- >>> import Data.List.NonEmpty (NonEmpty (..))
        -- >>> sconcat $ "Hello" :| [" ", "Haskell", "!"]
        -- "Hello Haskell!"
        sconcat :: NonEmpty a -> a
        sconcat (a :| as) = go a as where
          go b (c:cs) = b <> go c cs
          go b []     = b

        -- | Repeat a value @n@ times.
        --
        -- Given that this works on a 'Semigroup' it is allowed to fail if
        -- you request 0 or fewer repetitions, and the default definition
        -- will do so.
        --
        -- By making this a member of the class, idempotent semigroups
        -- and monoids can upgrade this to execute in \(\mathcal{O}(1)\) by
        -- picking @stimes = 'Data.Semigroup.stimesIdempotent'@ or @stimes =
        -- 'stimesIdempotentMonoid'@ respectively.
        --
        -- >>> stimes 4 [1]
        -- [1,1,1,1]
        stimes :: Integral b => b -> a -> a
        stimes = stimesDefault

        {-# MINIMAL (<>) | sconcat #-}

-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following:
--
-- [Right identity] @x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
-- [Associativity]  @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ ('Semigroup' law)
-- [Concatenation]  @'mconcat' = 'foldr' ('<>') 'mempty'@
--
-- You can alternatively define `mconcat` instead of `mempty`, in which case the
-- laws are:
--
-- [Unit]: @'mconcat' ('pure' x) = x@
-- [Multiplication]: @'mconcat' ('join' xss) = 'mconcat' ('fmap' 'mconcat' xss)@
-- [Subclass]: @'mconcat' ('toList' xs) = 'sconcat' xs@
--
-- The method names refer to the monoid of lists under concatenation,
-- but there are many other instances.
--
-- Some types can be viewed as a monoid in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Monoid', e.g. 'Data.Semigroup.Sum' and 'Data.Semigroup.Product'.
--
-- __NOTE__: 'Semigroup' is a superclass of 'Monoid' since /base-4.11.0.0/.
class Semigroup a => Monoid a where
        -- | Identity of 'mappend'
        --
        -- >>> "Hello world" <> mempty
        -- "Hello world"
        mempty :: a
        mempty = mconcat []
        {-# INLINE mempty #-}

        -- | An associative operation
        --
        -- __NOTE__: This method is redundant and has the default
        -- implementation @'mappend' = ('<>')@ since /base-4.11.0.0/.
        -- Should it be implemented manually, since 'mappend' is a synonym for
        -- ('<>'), it is expected that the two functions are defined the same
        -- way. In a future GHC release 'mappend' will be removed from 'Monoid'.
        mappend :: a -> a -> a
        mappend = (<>)
        {-# INLINE mappend #-}

        -- | Fold a list using the monoid.
        --
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.
        --
        -- >>> mconcat ["Hello", " ", "Haskell", "!"]
        -- "Hello Haskell!"
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty
        {-# INLINE mconcat #-}
        -- INLINE in the hope of fusion with mconcat's argument (see !4890)

        {-# MINIMAL mempty | mconcat #-}

-- | @since 4.9.0.0
instance Semigroup [a] where
        (<>) = (++)
        {-# INLINE (<>) #-}

        stimes = stimesList

-- | @since 2.01
instance Monoid [a] where
        {-# INLINE mempty #-}
        mempty  = []
        {-# INLINE mconcat #-}
        mconcat xss = [x | xs <- xss, x <- xs]
-- See Note: [List comprehensions and inlining]


{-
Note: [List comprehensions and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The list monad operations are traditionally described in terms of concatMap:

xs >>= f = concatMap f xs

Similarly, mconcat for lists is just concat. Here in Base, however, we don't
have concatMap, and we'll refrain from adding it here so it won't have to be
hidden in imports. Instead, we use GHC's list comprehension desugaring
mechanism to define mconcat and the Applicative and Monad instances for lists.
We mark them INLINE because the inliner is not generally too keen to inline
build forms such as the ones these desugar to without our insistence.  Defining
these using list comprehensions instead of foldr has an additional potential
benefit, as described in compiler/GHC/HsToCore/ListComp.hs: if optimizations
needed to make foldr/build forms efficient are turned off, we'll get reasonably
efficient translations anyway.
-}

-- | @since 4.9.0.0
instance Semigroup (NonEmpty a) where
        (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

-- | @since 4.9.0.0
instance Semigroup b => Semigroup (a -> b) where
        f <> g = \x -> f x <> g x
        stimes n f e = stimes n (f e)

-- | @since 2.01
instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        -- If `b` has a specialised mconcat, use that, rather than the default
        -- mconcat, which can be much less efficient.  Inline in the hope that
        -- it may result in list fusion.
        mconcat = \fs x -> mconcat (map (\f -> f x) fs)
        {-# INLINE mconcat #-}

-- | @since 4.9.0.0
instance Semigroup () where
        _ <> _      = ()
        sconcat _   = ()
        stimes  _ _ = ()

-- | @since 2.01
instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        mconcat _     = ()

-- | @since 4.15
instance Semigroup a => Semigroup (Solo a) where
  MkSolo a <> MkSolo b = MkSolo (a <> b)
  stimes n (MkSolo a) = MkSolo (stimes n a)

-- | @since 4.15
instance Monoid a => Monoid (Solo a) where
  mempty = MkSolo mempty

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
        (a,b) <> (a',b') = (a<>a',b<>b')
        stimes n (a,b) = (stimes n a, stimes n b)

-- | @since 2.01
instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
        (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
        stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
        (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
        stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
        (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
        stimes n (a,b,c,d,e) =
            (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)


-- | @since 4.9.0.0
instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

    stimes = stimesIdempotentMonoid

-- lexicographical ordering
-- | @since 2.01
instance Monoid Ordering where
    mempty             = EQ

-- | @since 4.9.0.0
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

    stimes = stimesMaybe

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\"
--
-- /Since 4.11.0/: constraint on inner @a@ value generalised from
-- 'Monoid' to 'Semigroup'.
--
-- @since 2.01
instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

-- | @since 4.10.0.0
instance Semigroup a => Semigroup (IO a) where
    -- Ideally we would define this as:
    --   (<>) = liftA2 (<>)
    -- but this would incur an import cycle.
    IO f <> IO g = IO (\s0 ->
      case f s0 of
        (# s1, x #) ->
          case g s1 of
            (# s2, y #) -> (# s2, x <> y #))

-- | @since 4.9.0.0
instance Monoid a => Monoid (IO a) where
    mempty = IO (\s -> (# s, mempty #) )

