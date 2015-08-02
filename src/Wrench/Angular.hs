{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
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

newtype Radians a = Radians { _radians :: a } deriving(Functor)

$(makePrisms ''Radians)

newtype Degrees a = Degrees { _degrees :: a } deriving(Functor)

$(makePrisms ''Degrees)

radians :: a -> Radians a
radians = Radians

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

degToRad :: Floating a => Degrees a -> Radians a
degToRad (Degrees x) = Radians (x * pi / 180)

radToDeg :: Floating a => Radians a -> Degrees a
radToDeg (Radians x) = Degrees (x * 180 / pi)

sinD :: Floating a => Degrees a -> a
sinD = sin . view _Radians . degToRad

cosD :: Floating a => Degrees a -> a
cosD = cos . view _Radians . degToRad

sinR :: Floating a => Radians a -> a
sinR = sin . view _Radians

cosR :: Floating a => Radians a -> a
cosR = cos . view _Radians
