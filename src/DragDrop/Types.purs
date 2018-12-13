module DragDrop.Types where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep as G
import Data.Lens (lens, view, set)
import Data.Generic.Rep.Show as GShow

type Card = {
  id :: Int,
  name :: String
}

type Cards = Array Card

data CardSection = Top | Bottom

type CardSections = {
  top :: Cards,
  bottom :: Cards
}

derive instance eqCardSection :: Eq CardSection
derive instance genericCardSection :: G.Generic CardSection _

instance showCardSection :: Show CardSection where
  show = GShow.genericShow

makeCard :: Int -> String -> Card
makeCard id name = { id, name }

type Model = {
  draggedCard :: Maybe Card,
  dropSectionTarget :: Maybe CardSection,
  sections :: CardSections
}

_sections = lens _.sections $ _ { sections = _ }
_top = lens _.top $ _ { top = _ }
_bottom = lens _.bottom $ _ { bottom = _ }

mTop = view $ _sections <<< _top
mBottom = view $ _sections <<< _bottom
setMTop newTop = set (_sections <<< _top) newTop
setMBottom newBottom = set (_sections <<< _bottom) newBottom
