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

type Card = {
  id :: Int,
  name :: String
}

makeCard :: Int -> String -> Card
makeCard id name = { id, name }

type Model = {
  topCards :: Array Card,
  bottomCards :: Array Card,
  draggedCard :: Maybe Card,
  dropSectionIsTarget :: Boolean
}

init :: Model
init = {
  topCards: [makeCard 1 "Get Schwifty", makeCard 2 "Hey Riiiiiick"],
  bottomCards: [],
  draggedCard: Nothing,
  dropSectionIsTarget: false
}

data Msg = CardDragStarted Card
         | CardDragEnded Card
         | CardDragged
         | DraggedOverDropSection
         | DroppedOnDropSection
         | SetDraggedCard Card
         | SetDropSectionTarget
         | ClearDropSectionTarget
         | AddCardToDropSection Card

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
  DraggedOverDropSection ->
    model :> [do
      _ <- H.sync $ Console.log "Dragged over drop section!"
      pure SetDropSectionTarget
    ]
  DroppedOnDropSection ->
    model :> [ do
      _ <- H.sync $ Console.log "Schlum-schulm...schlumalum"
      pure $ AddCardToDropSection $ makeCard 3 "Hallo"
    ]
  CardDragged ->
    model :> []
  SetDraggedCard card ->
    model { draggedCard = Just card } :> []
  SetDropSectionTarget ->
    model { dropSectionIsTarget = true } :> []
  ClearDropSectionTarget ->
    model { dropSectionIsTarget = false } :> []
  AddCardToDropSection card ->
    model { bottomCards = (Array.snoc model.bottomCards card) } :> []

{--$ viewCard <$> bottomCards,--}
view :: Model -> H.Html Msg
view model = H.main [H.id "main"] [
  H.div [H.class' "card-section"] $ viewCard <$> topCards,
  H.div [
    H.class' $ "card-section drop-section " <> droppingClass model.dropSectionIsTarget,
    onDrop DroppedOnDropSection,
    onDragOver DraggedOverDropSection
  ] $ viewCard <$> bottomCards
] where
  topCards = model.topCards
  bottomCards =  model.bottomCards

viewCard :: Card -> H.Html Msg
viewCard card =
  H.div [
    H.class' "card card-section-el",
    draggable true,
    onDragStart (CardDragStarted card),
    onDragEnd (CardDragEnded card)
  ] [
    H.text card.name
  ]

droppingClass :: Boolean -> String
droppingClass false = ""
droppingClass true = "dropping"

main :: Effect Unit
main = do
  Devtools.init
  H.mount "main" {
    init: init :> [],
    update,
    view
  }
