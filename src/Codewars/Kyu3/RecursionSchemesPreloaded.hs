{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types supplied by the Codewars environment for the "The Cata Kata".
-- On Codewars this module is preloaded as @RecursionSchemesPreloaded@; here we
-- keep it as a sibling module so the kata compiles locally.
module Codewars.Kyu3.RecursionSchemesPreloaded
  ( Fix (..)
  , Attr (..)
  ) where

-- | Fixed point of a functor.
newtype Fix f = Fix {unFix :: f (Fix f)}

deriving instance (Show (f (Fix f))) => Show (Fix f)

deriving instance (Eq (f (Fix f))) => Eq (Fix f)

deriving instance (Ord (f (Fix f))) => Ord (Fix f)

-- | Cofree-comonad-like annotation carrying the folded value plus history.
data Attr f a = Attr
  { value :: a -- This is what've already folded to
  , history :: f (Attr f a) -- This is every previous folding step
  }

deriving instance (Show (f (Attr f a)), Show a) => Show (Attr f a)
