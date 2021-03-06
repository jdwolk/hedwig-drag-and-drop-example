module Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Hedwig ((:>))
import Hedwig as H
import Hedwig.Devtools as Devtools
import Hedwig.Foreign (on, on', property)
import Web.Event.Event as Event

import DragDrop.Types

onDrag = on' "drag"
onDragEnd = on' "dragend"
onDragEnter = on' "dragenter"
onDragExit = on' "dragexit"
onDragLeave = on' "dragleave"
onDragStart = on' "dragstart"
onDrop = on' "drop"
-- Need to preventDefault so the onDrop will work
onDragOver msg = on "dragover" $ \event -> do
  Event.preventDefault event
  pure msg

draggable = property "draggable"

init :: Model
init = {
  draggedCard: Nothing,
  dropSectionTarget: Nothing,
  sections: {
    top: [ makeCard 1 "Get Schwifty"
         , makeCard 2 "Schlum-schlum-schlumalum"
         , makeCard 3 "Wubba-lubba-dub-dub!"
         , makeCard 4 "And that's the WAAAAY the news goes!"
         , makeCard 5 "A regular-old Plumbus"
         , makeCard 6 "Hit the sack, Jack!"
         ],
    bottom: []
  }
}

data Msg = CardDragStarted Card
         | CardDragEnded Card
         | CardDragged
         | DraggedOver CardSection
         | DroppedOnDropSection CardSection CardSection
         | SetDraggedCard Card
         | SetDropSectionTarget CardSection
         | ClearDropSectionTarget
         | ClearDraggedCard
         | MoveCardBetweenSections CardSection CardSection Card

removeCardFromCards :: Cards -> Card -> Cards
removeCardFromCards cards card = Array.filter (\c -> not (c.id == card.id)) cards

addCardToCards :: Cards -> Card -> Cards
addCardToCards cards card =
  case maybeFoundCard of
    Just _ -> cards
    Nothing -> Array.snoc cards card
  where maybeFoundCard = Array.findIndex (\c -> c.id == card.id) cards

update :: H.Update Model Msg
update model = case _ of
  CardDragStarted card ->
    model :> [ do
      _ <- H.sync $ Console.log $ "Dragging " <> (show card.name)
      pure $ SetDraggedCard card
    ]
  CardDragEnded _ ->
    model :> [ do
      _ <- H.sync $ Console.log "Wubba-lubba-dub-dub!"
      pure ClearDropSectionTarget
    ]
  DraggedOver cardSection ->
    model :> [do
      _ <- H.sync $ Console.log "Dragged over drop section!"
      pure $ SetDropSectionTarget cardSection
    ]
  DroppedOnDropSection from to ->
    model :>
      [ do
          let msg =
                case model.draggedCard of
                  Nothing -> ClearDropSectionTarget
                  Just card -> MoveCardBetweenSections from to card
          pure msg
      , pure ClearDraggedCard
      , pure ClearDropSectionTarget
      ]
  CardDragged ->
    model :> []
  SetDraggedCard card ->
    model { draggedCard = Just card } :> []
  SetDropSectionTarget cardSection->
    model { dropSectionTarget = Just cardSection } :> []
  ClearDraggedCard ->
    model { draggedCard = Nothing } :> []
  ClearDropSectionTarget ->
    model { dropSectionTarget = Nothing } :> []
  MoveCardBetweenSections Top Bottom card ->
    (setMTop (removeCardFromCards model.sections.top card) $
      (setMBottom (addCardToCards model.sections.bottom card) model)) :> []
  MoveCardBetweenSections Bottom Top card ->
    (setMTop (addCardToCards model.sections.top card) $
      (setMBottom (removeCardFromCards model.sections.bottom card) model)) :> []
  MoveCardBetweenSections _ _ card -> model :> []

view :: Model -> H.Html Msg
view model = H.main [H.id "main"] [
  H.div [
    H.class' $ "card-section drop-section " <> droppingClass Top model.dropSectionTarget,
    onDrop $ DroppedOnDropSection Bottom Top,
    onDragOver $ DraggedOver Top
  ] $ viewCard model.draggedCard <$> topCards,
  H.div [
    H.class' $ "card-section drop-section " <> droppingClass Bottom model.dropSectionTarget,
    onDrop $ DroppedOnDropSection Top Bottom,
    onDragOver $ DraggedOver Bottom
  ] $ viewCard model.draggedCard <$> bottomCards
] where
  topCards = model.sections.top
  bottomCards =  model.sections.bottom

viewCard :: Maybe Card -> Card -> H.Html Msg
viewCard maybeDraggedCard card =
  H.div [
    H.class' $ "card card-section-el " <> draggedCardClass card maybeDraggedCard,
    draggable true,
    onDragStart (CardDragStarted card),
    onDragEnd (CardDragEnded card)
  ] [
    H.div [H.class' "card-contents"] [
      H.text card.name
    ]
  ]

droppingClass :: CardSection -> Maybe CardSection -> String
droppingClass _ Nothing = ""
droppingClass cs (Just otherCs) = if cs == otherCs then "dropping" else ""

draggedCardClass :: Card -> Maybe Card -> String
draggedCardClass _ Nothing = ""
draggedCardClass card (Just otherCard) = if card == otherCard then "dragged" else ""

main :: Effect Unit
main = do
  Devtools.init
  H.mount "main" {
    init: init :> [],
    update,
    view
  }
