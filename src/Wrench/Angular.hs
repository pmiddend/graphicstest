{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Wrench.Angular where

import           ClassyPrelude
import           Control.Lens.Iso (Iso', iso)
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

