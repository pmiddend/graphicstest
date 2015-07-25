{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Angular where

import           ClassyPrelude
import           Control.Lens     ((^.))
import           Control.Lens.Iso (Iso', from, iso)
import           Control.Lens.TH  (makePrisms)

newtype Radians a = Radians { _radians :: a } deriving(Functor)

$(makePrisms ''Radians)

newtype Degrees a = Degrees { _degrees :: a } deriving(Functor)

$(makePrisms ''Degrees)

deriving instance Show a => Show (Degrees a)
deriving instance Num a => Num (Degrees a)
deriving instance Eq a => Eq (Degrees a)
deriving instance Ord a => Ord (Degrees a)

deriving instance Show a => Show (Radians a)
deriving instance Num a => Num (Radians a)
deriving instance Eq a => Eq (Radians a)
deriving instance Ord a => Ord (Radians a)

degrees :: Floating a => Iso' (Radians a) (Degrees a)
degrees = iso radToDeg degToRad

degToRad :: Floating a => Degrees a -> Radians a
degToRad (Degrees x) = Radians (x * pi / 180)

radToDeg :: Floating a => Radians a -> Degrees a
radToDeg (Radians x) = Degrees (x * 180 / pi)

sinD :: Floating a => Degrees a -> a
sinD x = sin (x ^. from degrees . _Radians)

cosD :: Floating a => Degrees a -> a
cosD x = cos (x ^. from degrees . _Radians)

sinR :: Floating a => Radians a -> a
sinR x = sin (x ^. _Radians)

cosR :: Floating a => Radians a -> a
cosR x = cos (x ^. _Radians)
