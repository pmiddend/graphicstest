{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-|
Module      : Wrench.Angular
Description : Contains newtypes and functions for radians/degrees
Maintainer  : pmidden@secure.mailbox.org
-}
module Wrench.Angular(
    Radians
  , Degrees
  , _Radians
  , _Degrees
  , radians
  , degrees
  , degToRad
  , radToDeg
  , sinD
  , cosD
  , sinR
  , cosR) where

import           ClassyPrelude
import           Control.Lens    (view)
import           Control.Lens.TH (makePrisms)

-- | Specifies a value in radians
newtype Radians a = Radians { _radians :: a } deriving(Functor)

$(makePrisms ''Radians)

-- | Specifies a value in degrees
newtype Degrees a = Degrees { _degrees :: a } deriving(Functor)

$(makePrisms ''Degrees)

-- | Converts a plain value to radians (no conversion is performed here)
radians :: a -> Radians a
radians = Radians

-- | Converts a plain value to degrees (no conversion is performed here)
degrees :: a -> Degrees a
degrees = Degrees

deriving instance Show a => Show (Degrees a)
deriving instance Num a => Num (Degrees a)
deriving instance Eq a => Eq (Degrees a)
deriving instance Ord a => Ord (Degrees a)
deriving instance Monoid a => Monoid (Degrees a)

deriving instance Show a => Show (Radians a)
deriving instance Num a => Num (Radians a)
deriving instance Eq a => Eq (Radians a)
deriving instance Ord a => Ord (Radians a)
deriving instance Monoid a => Monoid (Radians a)

-- | Converts degrees to radians
degToRad :: Floating a => Degrees a -> Radians a
degToRad (Degrees x) = Radians (x * pi / 180)

-- | Converts radians to degrees
radToDeg :: Floating a => Radians a -> Degrees a
radToDeg (Radians x) = Degrees (x * 180 / pi)

-- | Calculate the sine of a degree value
sinD :: Floating a => Degrees a -> a
sinD = sin . view _Radians . degToRad

-- | Calculate the cosine of a degree value
cosD :: Floating a => Degrees a -> a
cosD = cos . view _Radians . degToRad

-- | Calculate the sine of a radian value
sinR :: Floating a => Radians a -> a
sinR = sin . view _Radians

-- | Calculate the sine of a radian value
cosR :: Floating a => Radians a -> a
cosR = cos . view _Radians
