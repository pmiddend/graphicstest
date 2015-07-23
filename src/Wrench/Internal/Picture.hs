module Wrench.Internal.Picture where

import           ClassyPrelude
import qualified Data.Text                 as T
import           Linear.V2                 (V2)
import           Wrench.Angular
import           Wrench.Color
import           Wrench.RenderPositionMode
import           Wrench.SpriteIdentifier

data Picture a b = Text T.Text
                 | Sprite SpriteIdentifier RenderPositionMode (Maybe (V2 a))
                 | Blank
                 | InColor Color (Picture a b)
                 | Translate (V2 a) (Picture a b)
                 | Rotate (Radians b) (Picture a b)
                 | Scale (V2 a) (Picture a b)
                 | Pictures [(Picture a b)]
                 deriving(Show,Eq)

instance Semigroup (Picture a b) where
  a <> b = Pictures [a,b]

instance Monoid (Picture a b) where
  mempty = Blank
  mappend a b = Pictures [a,b]
