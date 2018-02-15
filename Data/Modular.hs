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

class Modular a where
  {-# MINIMAL pr, su #-}
  su :: a -> a
  pr :: a -> a

instance Modular a => Modular (Maybe a) where
  su Nothing = Nothing
  su (Just x) = Just (su x)
  pr Nothing = Nothing
  pr (Just x) = Just (pr x)

instance Integral a => Modular (Ratio a) where
  {-# SPECIALIZE instance Modular Rational #-}
  su x = x + 1
  pr x = x - 1

instance a ~ b => Modular (a :~: b) where
  su _ = Refl
  pr _ = Refl

instance a ~~ b => Modular (a :~~: b) where
  su _ = HRefl
  pr _ = HRefl

instance Modular () where
  su _ = ()
  pr _ = ()

instance (Modular a, Modular b) => Modular (a,b) where
  su (a,b) = (su a, su b)
  pr (a,b) = (pr a, pr b)

instance (Modular a, Modular b) => Modular (Either a b) where
  su (Left a) = Left $ (su a)
  su (Right b) = Right $ (su b)

  pr (Left a) = Left $ (pr a)
  pr (Right b) = Right $ (pr b)

instance Modular Bool where
  su False = True
  su _     = False

  pr True  = False
  pr _     = True

instance Modular Ordering where
  su LT = EQ
  su EQ = GT
  su GT = LT

  pr GT = EQ
  pr EQ = LT
  pr LT = GT

instance Modular Integer where
  su x = x + 1
  pr x = x - 1

instance Modular Int where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1


instance Modular Int8 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int16 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int32 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Int64 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1
  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word8 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word16 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word32 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1

instance Modular Word64 where
  su x
    | x == maxBound = minBound
    | otherwise     = x + 1

  pr x
    | x == minBound = maxBound
    | otherwise     = x - 1
