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
  dropSectionTarget :: Maybe CardSection
}

data CardSection = Top | Bottom

derive instance eqCardSection :: Eq CardSection

init :: Model
init = {
  topCards: [ makeCard 1 "Get Schwifty"
            , makeCard 2 "Schlum-schlum-schlumalum"
            , makeCard 3 "Wubba-lubba-dub-dub!"
            , makeCard 4 "And that's the WAAAAY the news goes!"
            ],
  bottomCards: [],
  draggedCard: Nothing,
  dropSectionTarget: Nothing
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

removeCardFromCards :: Array Card -> Card -> Array Card
removeCardFromCards cards card = Array.filter (\c -> not (c.id == card.id)) cards

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
    model
    { bottomCards = case Array.findIndex (\c -> c.id == card.id) model.bottomCards of
                      Just _ -> model.bottomCards
                      Nothing -> (Array.snoc model.bottomCards card)
    , topCards = removeCardFromCards model.topCards card
    } :> []
  MoveCardBetweenSections Bottom Top card ->
    model
    { bottomCards = removeCardFromCards model.bottomCards card
    , topCards = case Array.findIndex (\c -> c.id == card.id) model.topCards of
                      Just _ -> model.topCards
                      Nothing -> (Array.snoc model.topCards card)
    } :> []
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
  topCards = model.topCards
  bottomCards =  model.bottomCards

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
