{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Modular
  ( Modular(..)
  ) where

import Data.Either
import Data.Maybe
import Data.Type.Equality
import GHC.Base
import GHC.Enum (Bounded(..))
import GHC.Int
import GHC.Num
import GHC.Real
import GHC.Word

-- | Similar to 'Discrete' but 'msucc' and 'mpred' experience 'wrap-around' for
--   'Bounded' types.
-- 
--   @'msucc' . 'mpred' = 'id'@
--   @'mpred' . 'msucc' = 'id'@
--   @'msucc' 'maxBound' = 'minBound'@
--   @'mpred' 'minBound' = 'maxBound'@
class Modular a where
  {-# MINIMAL mpred, msucc #-}
  msucc :: a -> a
  mpred :: a -> a

instance Modular a => Modular (Maybe a) where
  msucc Nothing = Nothing
  msucc (Just x) = Just (msucc x)
  mpred Nothing = Nothing
  mpred (Just x) = Just (mpred x)

instance Integral a => Modular (Ratio a) where
  {-# SPECIALIZE instance Modular Rational #-}
  msucc x = x + 1
  mpred x = x - 1

instance a ~ b => Modular (a :~: b) where
  msucc _ = Refl
  mpred _ = Refl

instance a ~~ b => Modular (a :~~: b) where
  msucc _ = HRefl
  mpred _ = HRefl

instance Modular () where
  msucc _ = ()
  mpred _ = ()

instance (Modular a, Modular b) => Modular (a,b) where
  msucc (a,b) = (msucc a, msucc b)
  mpred (a,b) = (mpred a, mpred b)

instance (Modular a, Modular b) => Modular (Either a b) where
  msucc (Left a) = Left $ (msucc a)
  msucc (Right b) = Right $ (msucc b)

  mpred (Left a) = Left $ (mpred a)
  mpred (Right b) = Right $ (mpred b)

instance Modular Bool where
  msucc False = True
  msucc _     = False

  mpred True  = False
  mpred _     = True

instance Modular Ordering where
  msucc LT = EQ
  msucc EQ = GT
  msucc GT = LT

  mpred GT = EQ
  mpred EQ = LT
  mpred LT = GT

instance Modular Integer where
  msucc x = x + 1
  mpred x = x - 1

instance Modular Int where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1


instance Modular Int8 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int16 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int32 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int64 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1
  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word8 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word16 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word32 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word64 where
  msucc x
    | x == maxBound = minBound
    | otherwise     = x + 1

  mpred x
    | x == minBound = maxBound
    | otherwise     = x - 1
