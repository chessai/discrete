{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Discrete
  ( Discrete(..)
  ) where

import Control.Applicative ((<$>))
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Functor.Const
import Data.Functor.Identity
import Data.Type.Equality
import GHC.Base
import GHC.Enum (Bounded(..))
import GHC.Int
import GHC.Num
import GHC.Real
import GHC.Word

class Discrete a where
  {-# MINIMAL pr, su #-}
  su :: a -> Maybe a
  pr :: a -> Maybe a

deriving newtype instance Discrete (f a) => Discrete (Alt f a)
deriving newtype instance Discrete a => Discrete (Identity a)
deriving newtype instance Discrete a => Discrete (Const a b)

instance Discrete a => Discrete (Maybe a) where
  su Nothing  = Nothing
  su (Just x) = Just (su x)
  pr Nothing  = Nothing
  pr (Just x) = Just (pr x)

instance Integral a => Discrete (Ratio a) where
  {-# SPECIALIZE instance Discrete Rational #-} 
  su x = Just $ x + 1
  pr x = Just $ x - 1

instance a ~ b => Discrete (a :~: b) where
  su _ = Just Refl
  pr _ = Just Refl

instance a ~~ b => Discrete (a :~~: b) where
  su _ = Just HRefl
  pr _ = Just HRefl

instance Discrete () where
  su _ = Just ()
  pr _ = Just ()

instance (Bounded b, Discrete a, Discrete b) => Discrete (a,b) where
  su (a,b) = maybe (flip (,) minBound <$> su a) (Just . (,) a) (su b)
  pr (a,b) = maybe (flip (,) maxBound <$> pr a) (Just . (,) a) (pr b)

instance (Bounded a, Bounded b, Discrete a, Discrete b) => Discrete (Either a b) where
  su (Left a) = maybe (Just $ Right minBound) (Just . Left) (su a)
  su (Right b) = maybe (Nothing) (Just . Right) (su b)
  
  pr (Left a) = maybe (Nothing) (Just . Left) (pr a)
  pr (Right b) = maybe (Just $ Left maxBound) (Just . Right) (pr b)

instance Discrete Bool where
  su False = Just True
  su _     = Nothing

  pr True  = Just False
  pr _     = Nothing

instance Discrete Ordering where
  su LT = Just EQ
  su EQ = Just GT
  su GT = Nothing

  pr GT = Just EQ
  pr EQ = Just LT
  pr LT = Nothing

instance Discrete Char where
  su (C# c#)
    | isTrue# (ord# c# /=# 0x10FFFF#) = Just $ C# (chr# (ord# c# +# 1#))
    | otherwise = Nothing
  pr (C# c#)
    | isTrue# (ord# c# /=# 0#) = Just $ C# (chr# (ord# c# -# 1#))
    | otherwise = Nothing

instance Discrete Integer where
  su x = Just $ x + 1
  pr x = Just $ x - 1

instance Discrete Int where
  su x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pr x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int8 where
  su x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pr x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int16 where
  su x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pr x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int32 where
  su x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pr x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int64 where
  su x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pr x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Word where
  su x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pr x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

instance Discrete Word8 where
    su x
        | x /= maxBound = Just $ x + 1
        | otherwise     = Nothing
    pr x
        | x /= minBound = Just $ x - 1
        | otherwise     = Nothing

instance Discrete Word16 where
    su x
        | x /= maxBound = Just $ x + 1
        | otherwise     = Nothing
    pr x
        | x /= minBound = Just $ x - 1
        | otherwise     = Nothing

instance Discrete Word32 where
    su x
        | x /= maxBound = Just $ x + 1
        | otherwise     = Nothing
    pr x
        | x /= minBound = Just $ x - 1
        | otherwise     = Nothing

instance Discrete Word64 where
    su x
        | x /= maxBound = Just $ x + 1
        | otherwise     = Nothing
    pr x
        | x /= minBound = Just $ x - 1
        | otherwise     = Nothing

