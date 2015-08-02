{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Wrench.Internal.Picture where

import           ClassyPrelude
import qualified Data.Text               as T
import           Linear.V2
import           Wrench.Angular
import           Wrench.Color
import           Wrench.SpriteIdentifier

data Picture unit float =
    Blank
  | Text T.Text
  | Sprite SpriteIdentifier (V2 unit)
  | InColor Color (Picture unit float)
  | Translate (V2 unit) (Picture unit float)
  | Rotate (Radians float) (Picture unit float)
  | Scale (V2 unit) (Picture unit  float)
  | Pictures [Picture unit float]

instance Bifunctor Picture where
  bimap _ _ Blank = Blank
  bimap _ _ (Text t) = Text t
  bimap f _ (Sprite identifier pos) = Sprite identifier (f <$> pos)
  bimap f g (InColor color subImage) = InColor color (bimap f g subImage)
  bimap f g (Translate p subImage) = Translate (fmap f p) (bimap f g subImage)
  bimap f g (Rotate rads subImage) = Rotate (fmap g rads) (bimap f g subImage)
  bimap f g (Scale p subImage) = Scale (fmap f p) (bimap f g subImage)
  bimap f g (Pictures ps) = Pictures ((bimap f g) <$> ps)

deriving instance (Show unit,Show float) => Show (Picture unit float)

instance Semigroup (Picture unit float) where
  a <> b = Pictures [a,b]

instance Monoid (Picture unit float) where
  mempty = Blank
  mappend a b = Pictures [a,b]
