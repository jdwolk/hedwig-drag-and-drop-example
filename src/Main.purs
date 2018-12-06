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
  topCards: [makeCard 1 "Get Schwifty", makeCard 2 "Hey Riiiiiick", makeCard 3 "Schlum-schlum-schlumalum"],
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
         | MoveCardBetweenSections CardSection CardSection Card

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
    model :> [ do
      let hello =
            case model.draggedCard of
              Nothing -> ClearDropSectionTarget
              Just card -> MoveCardBetweenSections from to card
      pure hello
    ]
  CardDragged ->
    model :> []
  SetDraggedCard card ->
    model { draggedCard = Just card } :> []
  SetDropSectionTarget cardSection->
    model { dropSectionTarget = Just cardSection } :> []
  ClearDropSectionTarget ->
    model { dropSectionTarget = Nothing } :> []
  MoveCardBetweenSections Top Bottom card ->
    model
    { bottomCards = case Array.findIndex (\c -> c.id == card.id) model.bottomCards of
                      Just _ -> model.bottomCards
                      Nothing -> (Array.snoc model.bottomCards card)
    , topCards = Array.filter (\c -> not (c.id == card.id) ) model.topCards
    } :> []
  MoveCardBetweenSections Bottom Top card ->
    model
    { bottomCards = Array.filter (\c -> not (c.id == card.id) ) model.bottomCards
    , topCards = case Array.findIndex (\c -> c.id == card.id) model.topCards of
                      Just _ -> model.topCards
                      Nothing -> (Array.snoc model.topCards card)
    } :> []
  MoveCardBetweenSections _ _ card -> model :> []


  {--AddCardToDropSection card ->                                       --}
  {--  model { bottomCards = (Array.snoc model.bottomCards card) } :> []--}

view :: Model -> H.Html Msg
view model = H.main [H.id "main"] [
  H.div [
    H.class' "card-section",
    onDrop $ DroppedOnDropSection Bottom Top,
    onDragOver $ DraggedOver Top
  ] $ viewCard <$> topCards,
  H.div [
    H.class' $ "card-section drop-section " <> droppingClass Bottom model.dropSectionTarget,
    onDrop $ DroppedOnDropSection Top Bottom,
    onDragOver $ DraggedOver Bottom
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

droppingClass :: CardSection -> Maybe CardSection -> String
droppingClass _ Nothing = ""
droppingClass cs (Just otherCs) = if cs == otherCs then "dropping" else ""

main :: Effect Unit
main = do
  Devtools.init
  H.mount "main" {
    init: init :> [],
    update,
    view
  }
