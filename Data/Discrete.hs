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
import Control.Monad ((>=>))

-- | A 'Discrete' type is a set X with at least one element, along with two
--   functions, @'succ' :: X -> 'Maybe' X@ and @'pred' :: X -> 'Maybe' X@,
--   such that all inhabitants of the set X can be constructed given at least
--   a single element of the set and these two functions. The following must hold:
--   
--   @'pred' '>=>' 'succ' '>=>' 'pred' = 'pred'@
--
--   @'succ' '>=>' 'pred' '>=>' 'succ' = 'succ'@ 
--
--   This means that 'Int' is a discrete type, because given any x :: 'Int', one
--   can construct any other 'Int' with the following functions:
-- 
--     @'succ' x = if x '==' 'maxBound' then 'Nothing' else 'Just' (x '+' 1)@
--
--     @'pred' x = if x '==' 'minBound' then 'Nothing' else 'Just' (x '-' 1)@
--
--   This also means that something like 'Double' is /not/ a discrete type, because
--   there are no such functions 'succ' and 'pred' that given any value of type 'Double'
--   can allow the construction of all values of type 'Double'.
--
--   'Discrete' acts as a replacement for 'GHC.Enum.Enum'. The motivation for
--   'Discrete' is two-fold: firstly, totality of all typeclass instances, and
--   secondly, that 'GHC.Enum.Enum' is a typeclass that does too many things,
--   and does them poorly. If 'succ' or 'pred' are called on 'maxBound'
--   or 'minBound', respectively, the result will be 'Nothing'.
class Discrete a where
  {-# MINIMAL pred, succ #-}
  succ :: a -> Maybe a
  pred :: a -> Maybe a

deriving newtype instance Discrete (f a) => Discrete (Alt f a)
deriving newtype instance Discrete a => Discrete (Identity a)
deriving newtype instance Discrete a => Discrete (Const a b)

instance Discrete a => Discrete (Maybe a) where
  succ Nothing  = Nothing
  succ (Just x) = Just (succ x)
  pred Nothing  = Nothing
  pred (Just x) = Just (pred x)

instance Integral a => Discrete (Ratio a) where
  {-# SPECIALIZE instance Discrete Rational #-} 
  succ x = Just $ x + 1
  pred x = Just $ x - 1

instance a ~ b => Discrete (a :~: b) where
  succ _ = Just Refl
  pred _ = Just Refl

instance a ~~ b => Discrete (a :~~: b) where
  succ _ = Just HRefl
  pred _ = Just HRefl

instance Discrete () where
  succ _ = Just ()
  pred _ = Just ()

instance (Bounded b, Discrete a, Discrete b) => Discrete (a,b) where
  succ (a,b) = maybe (flip (,) minBound <$> succ a) (Just . (,) a) (succ b)
  pred (a,b) = maybe (flip (,) maxBound <$> pred a) (Just . (,) a) (pred b)

instance (Bounded a, Bounded b, Discrete a, Discrete b) => Discrete (Either a b) where
  succ (Left a) = maybe (Just $ Right minBound) (Just . Left) (succ a)
  succ (Right b) = maybe (Nothing) (Just . Right) (succ b)
  
  pred (Left a) = maybe (Nothing) (Just . Left) (pred a)
  pred (Right b) = maybe (Just $ Left maxBound) (Just . Right) (pred b)

instance Discrete Bool where
  succ False = Just True
  succ _     = Nothing

  pred True  = Just False
  pred _     = Nothing

instance Discrete Ordering where
  succ LT = Just EQ
  succ EQ = Just GT
  succ GT = Nothing

  pred GT = Just EQ
  pred EQ = Just LT
  pred LT = Nothing

instance Discrete Char where
  succ (C# c#)
    | isTrue# (ord# c# /=# 0x10FFFF#) = Just $ C# (chr# (ord# c# +# 1#))
    | otherwise = Nothing
  pred (C# c#)
    | isTrue# (ord# c# /=# 0#) = Just $ C# (chr# (ord# c# -# 1#))
    | otherwise = Nothing

instance Discrete Integer where
  succ x = Just $ x + 1
  pred x = Just $ x - 1

instance Discrete Int where
  succ x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pred x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int8 where
  succ x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pred x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int16 where
  succ x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pred x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int32 where
  succ x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pred x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Int64 where
  succ x
    | x == maxBound = Nothing
    | otherwise     = Just $ x + 1
  
  pred x
    | x == minBound = Nothing
    | otherwise     = Just $ x - 1

instance Discrete Word where
  succ x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pred x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

instance Discrete Word8 where
  succ x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pred x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

instance Discrete Word16 where
  succ x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pred x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

instance Discrete Word32 where
  succ x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pred x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

instance Discrete Word64 where
  succ x
    | x /= maxBound = Just $ x + 1
    | otherwise     = Nothing
  pred x
    | x /= minBound = Just $ x - 1
    | otherwise     = Nothing

