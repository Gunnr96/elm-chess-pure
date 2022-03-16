module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, h2, text, div)
import Html.Attributes exposing (style, class, attribute)

import Html.Events as Event
import Json.Decode as D

import Rank exposing (..)
import File exposing (..)
import Piece exposing (..)

import Board exposing (Board)
import Board as Board

import Vector4 exposing (Vector4)


type alias Tile = { position : Position, color : TileColor }

type TileColor =
  Dark
  | Light
  | Red
  | Yellow
  | Blue

type alias Model =
  { board : Board
  , movingPiece : Maybe (Position, Piece)
  , playerToMove : Color
  , playerInCheck : Maybe Color
  , tiles : List Tile
  }

type Msg
  = NoOp
  | Move Position Piece
  | CancelMove
  | DropOn Position
  | Highlight Position

flip : Color -> Color
flip color =
  case color of
    Black -> White
    White -> Black

cssTileColor : TileColor -> String
cssTileColor color =
  case color of
    Dark -> "dark"
    Light -> "light"
    Red -> "red"
    Yellow -> "yellow"
    Blue -> "blue"

cssColor : Color -> String
cssColor color =
  case color of
    White -> "light"
    Black -> "dark"

positionCSS : Position -> String
positionCSS position = "tile-" ++ File.asString position.file ++ Rank.asString position.rank


onDragStart : msg -> Attribute msg
onDragStart message =
  Event.preventDefaultOn "mousedown" (D.succeed (message, True))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    Move position piece -> 
      let
          legalMovePositions = getLegalMoves_ position piece model.board

          newTiles =
            model.tiles
            |> List.map (\tile ->
              if List.member tile.position legalMovePositions
              then { tile | color = Red }
              else { tile | color = tileColor tile.position}
            )
      in
      
      ( { model | movingPiece = Just (position, piece), tiles = newTiles } , Cmd.none)

    CancelMove ->
      ( { model | movingPiece = Nothing }, Cmd.none)

    DropOn tile ->
      let
        newModel =
          model.movingPiece
          |> Maybe.map (\(position, movingPiece) ->
              if movingPiece.color == model.playerToMove && List.member tile (getLegalMoves_ position movingPiece model.board)
              then { model
                   | board = model.board
                            |> Board.move position tile
                   , playerToMove = flip model.playerToMove
                   , playerInCheck =
                    case (isInCheck White model.board, isInCheck Black model.board) of
                      (True, _) -> Just White
                      (_, True) -> Just Black
                      (False, False) -> Nothing
              }
              else model
            )
          |> Maybe.withDefault model
      in
      
      (newModel , Cmd.none)

    Highlight position ->
      let
          diagonals = Board.diagonals position |> Vector4.foldr (++) []
          lines = Board.lines position |> Vector4.foldr (++) []

          knightMoves = Board.knightMoves position

          newTiles = 
            model.tiles
            |> List.map (\tile ->
                        case (List.member tile.position lines, List.member tile.position diagonals, List.member tile.position knightMoves) of
                          (True, _, _) -> { tile | color = tileColor tile.position }
                          (_, True, _) -> { tile | color = tileColor tile.position }
                          (_, _, True) -> { tile | color = tileColor tile.position }
                          (_, _, _)    -> { tile | color = tileColor tile.position }
                        )
      in
        ({ model | tiles = newTiles }, Cmd.none)     


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

board : Model -> Html Msg
board model =
  let
    normal piece position =
        div 
          [ class (asCSSClass piece)
          , class "piece"
          , class (positionCSS position)
          , Event.onMouseUp (DropOn position)
          , onDragStart (Move position piece)
          ] []
    moving piece position =
      div
        [ class (asCSSClass piece)
        , class "piece"
        , class (positionCSS position)
        , style "opacity" "0.1"
        ] [ ]
    showPiece (position, piece) =
      case model.movingPiece of
        Nothing ->
          normal piece position
        Just (movingPiecePosition, movingPiece) ->
          if movingPiecePosition /= position then 
          normal piece position
          else
          moving piece position

    tiles =
      model.tiles
      |> List.map (\tile ->
          div [ class (positionCSS tile.position)
        , class "tile"
        , class <| cssTileColor tile.color
        , Event.onMouseUp (DropOn tile.position)
        , Event.onClick (Highlight tile.position)
        ] []
      )
    playerInCheck = 
      case model.playerInCheck of
        Nothing -> div [ class "indicator" ] []
        Just color -> div [ class "indicator", class <| cssColor color ] [] 
  in
    div []
    [ div [ class "indicator", class <| cssColor model.playerToMove ] [ ]
    , playerInCheck
    , div [] (tiles ++ (Board.asIndexedList model.board |> List.map showPiece))
    ]
    

view : Model -> Html Msg
view model = board model

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : () -> ( Model, Cmd Msg )
init _ =
  let
   tiles = [
      Position A Eight  , Position B Eight  , Position C Eight  , Position D Eight  , Position E Eight  , Position F Eight  , Position G Eight  , Position H Eight, 
      Position A Seven  , Position B Seven  , Position C Seven  , Position D Seven  , Position E Seven  , Position F Seven  , Position G Seven  , Position H Seven, 
      Position A Six    , Position B Six    , Position C Six    , Position D Six    , Position E Six    , Position F Six    , Position G Six    , Position H Six, 
      Position A Five   , Position B Five   , Position C Five   , Position D Five   , Position E Five   , Position F Five   , Position G Five   , Position H Five, 
      Position A Four   , Position B Four   , Position C Four   , Position D Four   , Position E Four   , Position F Four   , Position G Four   , Position H Four, 
      Position A Three  , Position B Three  , Position C Three  , Position D Three  , Position E Three  , Position F Three  , Position G Three  , Position H Three, 
      Position A Two    , Position B Two    , Position C Two    , Position D Two    , Position E Two    , Position F Two    , Position G Two    , Position H Two, 
      Position A One    , Position B One    , Position C One    , Position D One    , Position E One    , Position F One    , Position G One    , Position H One]
    |> List.map (\position ->
      Tile position (tileColor position)
    )
  in
    ( { board = Board.startingBoard
      , movingPiece = Nothing
      , playerToMove = White
      , playerInCheck = Nothing
      , tiles = tiles
      }
    , Cmd.none
    )

tileColor : Position -> TileColor
tileColor position = if modBy 2 (File.asInt position.file + Rank.asInt position.rank) == 1 then Light else Dark

-- |> List.map (\position ->
--   div [ class (positionCSS position)
--       , class "tile"
--       , class (if modBy 2 (File.asInt position.file + Rank.asInt position.rank) == 1 then "light" else "dark")
--       , Event.onMouseUp (DropOn position)
--       , Event.onClick (Highlight position)
--       ] [])

getLegalMoves_ : Position -> Piece -> Board -> List Position
getLegalMoves_ position piece board_ = 
  let

    lines = moves piece (Board.lines position) board_
    diagonals = moves piece (Board.diagonals position) board_

    forwardDirection =
        case piece.color of
          White -> Rank.succ
          Black -> Rank.pred

    one f = f
    two f = Maybe.andThen f << f

    pieceInFront = 
      forwardDirection position.rank
      |> Maybe.map (\rank -> Position position.file rank)
      |> Maybe.andThen (\inFront -> Board.get inFront board_ )
    
    pieceInFront2 =
        forwardDirection position.rank
        |> Maybe.andThen Rank.succ
        |> Maybe.map (\rank -> Position position.file rank)
        |> Maybe.andThen (\inFront -> Board.get inFront board_)
    
    pieceInFrontLeft =
      Maybe.map2
        Position
        (File.succ position.file)
        (forwardDirection position.rank)
      |> Maybe.andThen (\inFrontLeft -> Board.get inFrontLeft board_)

    pieceInFrontRight =
      Maybe.map2
        Position
        (File.pred position.file)
        (forwardDirection position.rank)
      |> Maybe.andThen (\inFrontRight -> Board.get inFrontRight board_)

  in
  case piece.pieceType of
    Pawn    ->
      []

      
      -- if Piece.startingRank piece == position.rank
      -- then
      --   if piece.color == White
      --   then [Position (position.file) (Three), Position (position.file) (Four)]
      --   else [Position (position.file) (Six), Position (position.file) (Five)]
      -- else
      --   [Position (position.file) (forwardDirection position.rank |> Maybe.withDefault position.rank)] 
    Rook    ->
      lines |> Vector4.foldr (++) []
    Knight  ->
      Board.knightMoves position
    Bishop  ->
      diagonals |> Vector4.foldr (++) []
    Queen   ->
      (lines |> Vector4.foldr (++) []) ++ (diagonals |> Vector4.foldr (++) [])
    King    ->
      (lines |> Vector4.map (List.take 1) |> Vector4.foldr (++) [])
      ++
      (diagonals |> Vector4.map (List.take 1) |> Vector4.foldr (++) [])

isInCheck : Color -> Board -> Bool
isInCheck color board_ =
  let
    king = 
      board_
      |> Board.asIndexedList
      |> find (\(_, piece) ->
            piece.pieceType == King && piece.color == color
          )

    testMoves p fromPos = getLegalMoves_ fromPos (Piece color p) board_
      |> List.map(\it -> Board.get it board_)
      |> List.any (\mPiece ->
                    mPiece
                    |> Maybe.map (\piece -> piece.color == flip color && piece.pieceType == p)
                    |> Maybe.withDefault False
                  )
  in
    king
    |> Maybe.map (\(kingLocation, _) ->
      testMoves Bishop kingLocation
      || testMoves Rook kingLocation
      || testMoves Queen kingLocation
      || testMoves Knight kingLocation
      || testMoves King kingLocation
      || testMoves Pawn kingLocation
    )
    |> Maybe.withDefault False


-- * HELPER FUNCTIONS

type Terminate = Keep | Toss
type Traversal = Continue | Stop Terminate 

moves :  Piece -> Vector4 (List Position) -> Board -> Vector4 (List Position)
moves piece validMoves board_ =
  validMoves 
    |> Vector4.map 
      (takeWhile 
        (\x ->
          -- color of the piece being moved, some other piece on the board
          case (piece.color, Board.get x board_ |> Maybe.map(\it -> it.color)) of
            (_    , Nothing)        -> Continue
            (White, Just White) -> Stop Toss
            (White, Just Black) -> Stop Keep
            (Black, Just White) -> Stop Keep
            (Black, Just Black) -> Stop Toss
        )
      )

takeWhile : (a -> Traversal) -> List a -> List a
takeWhile func list =
  case list of
    (x::xs) -> 
      case func x of
        -- continue
        Continue -> x :: takeWhile func xs
        -- stop
        Stop Toss -> []
        -- stop but include current element
        Stop Keep -> [x]
    [] -> []

find : (a -> Bool) -> List a -> Maybe a
find fun list =
  case list of
    [] -> Nothing
    (x::xs) -> if fun x then Just x else find fun xs
