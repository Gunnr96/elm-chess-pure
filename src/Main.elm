module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style, class)

import Html.Events as Event
import Json.Decode as D

import Rank exposing (..)
import File exposing (..)
import Piece exposing (..)

import MaybeExtra

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
  | MakeMove Position Piece
  | CancelMove
  | DropOn Position
  | Highlight Position


type MoveType
  = Reposition
  | Capture

type alias Move =
  { position : Position
  , moveType : MoveType
  }

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
    MakeMove position piece -> 
      let
          legalMovePositions =
            getLegalMoves_ position piece model.board
            |> List.map (\move -> move.position)

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
              let
                newBoard = model.board
                            |> Board.move position tile
              in
              if movingPiece.color == model.playerToMove
                  && List.member tile (getLegalMoves_ position movingPiece model.board
                                        |> List.map (\move -> move.position)
                                      )
              then { model
                   | board = newBoard
                   , playerToMove = flip model.playerToMove
                   , playerInCheck =
                    case (isInCheck White newBoard, isInCheck Black newBoard) of
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
subscriptions _ =
  Sub.none

boardView : Model -> Html Msg
boardView model =
  let
    normal piece position =
        div 
          [ class (asCSSClass piece)
          , class "piece"
          , class (positionCSS position)
          , Event.onMouseUp (DropOn position)
          , onDragStart (MakeMove position piece)
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
        Just (movingPiecePosition, _) ->
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

  in
    div [ class "board" ]
    (tiles ++ (Board.asIndexedList model.board |> List.map showPiece))
    
indicators : Model -> Html Msg
indicators model = 
  let
    playerInCheck = 
      case model.playerInCheck of
        Nothing -> div [ class "indicator" ] []
        Just color -> div [ class "indicator", class <| cssColor color ] [] 
  in
  div [ class "indicator-panel"] [
    div [ class "indicator"
      , class <| cssColor model.playerToMove
      ] [ ]
    , playerInCheck
  ]
  
view : Model -> Html Msg
view model = 
  div [] 
  [ boardView model
  , indicators model
  ]
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


positionOffset : Int -> Int -> Position -> Maybe Position
positionOffset x y pos =
  Maybe.map2 Position (File.offset x pos.file) (Rank.offset y pos.rank)

moveTo : Position -> Move
moveTo pos = Move pos Reposition

capture : Position -> Move
capture pos = Move pos Capture

getLegalMoves_ : Position -> Piece -> Board -> List Move
getLegalMoves_ position piece board = 
  let

    lines = moves piece (Board.lines position) board
    diagonals = moves piece (Board.diagonals position) board

    basisVector =
      case piece.color of
          White -> 1
          Black -> -1

  in
  case piece.pieceType of
    Pawn    ->
      let
          regularMoves =
            [ positionOffset 0 basisVector position
            , (if Piece.startingRank piece == position.rank then positionOffset 0 (2 * basisVector) position else Nothing)
            ] |> List.map (\mPos ->
                            mPos
                            |> Maybe.andThen (\pos -> Board.get pos board)
                            |> (\x -> 
                                case x of
                                  Nothing -> mPos |> Maybe.map (\pos -> Move pos Reposition)
                                  Just _ -> Nothing
                              )
                          )
          capturingMoves =
            [ positionOffset basisVector basisVector position
            , positionOffset (basisVector * -1) basisVector position
            ] |> List.map (\mPos ->
                            mPos
                            |> Maybe.andThen (\pos -> Board.get pos board)
                            |> Maybe.andThen (\_ -> mPos |> Maybe.map (\pos -> Move pos Capture))
                          )
      in
        MaybeExtra.catMaybes (regularMoves ++ capturingMoves)
       
    Rook    ->
      lines |> Vector4.foldr (++) []
    Knight  ->
      Board.knightMoves position
        |> List.map (\pos -> 
                      Board.get pos board
                        |> Maybe.andThen (\otherPiece -> 
                                            if piece.color == otherPiece.color
                                            then Nothing
                                            else Just <| capture pos
                                         )
                     )
        |> MaybeExtra.catMaybes
    Bishop  ->
      diagonals |> Vector4.foldr (++) []
    Queen   ->
      (lines |> Vector4.foldr (++) []) ++ (diagonals |> Vector4.foldr (++) [])
    King    ->
      (lines |> Vector4.map (List.take 1) |> Vector4.foldr (++) [])
      ++
      (diagonals |> Vector4.map (List.take 1) |> Vector4.foldr (++) [])

isInCheck : Color -> Board -> Bool
isInCheck color board =
  let
    king = 
      board
      |> Board.asIndexedList
      |> find (\(_, piece) ->
            piece.pieceType == King && piece.color == color
          )

    testMoves p fromPos = getLegalMoves_ fromPos (Piece color p) board
      |> List.filter (\move -> move.moveType == Capture)
      |> List.map (\move -> Board.get move.position board)
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

moves :  Piece -> Vector4 (List Position) -> Board -> Vector4 (List Move)
moves piece validMoves board =
  validMoves 
    |> Vector4.map 
      (takeWhile 
        (\x ->
          -- color of the piece being moved, some other piece on the board
          case (piece.color, Board.get x board) of
            (_    , Nothing)  -> True
            (_, Just _)       -> False
        )
      ) |> Vector4.map (\(list, mLast) -> 
            List.map moveTo list
            ++ (mLast
                  |> Maybe.andThen (\last -> Board.get last board |> Maybe.map (\p -> (last, p)))
                  |> Maybe.andThen (\(otherPiecePosition, otherPiece) ->
                        if piece.color == otherPiece.color
                        then Nothing
                        else Just <| capture otherPiecePosition
                    )
                  |> MaybeExtra.maybeToList
                )
          )

-- returns elements while predicate is accepted
-- and the last accepted element separately
takeWhile : (a -> Bool) -> List a -> (List a, Maybe a)
takeWhile func list =
  case list of
    (x::xs) ->
      if func x 
        -- continue
      then (x :: Tuple.first (takeWhile func xs), Nothing)
        -- stop
      else ([], Just x)
    [] -> ([], Nothing)

find : (a -> Bool) -> List a -> Maybe a
find fun list =
  case list of
    [] -> Nothing
    (x::xs) -> if fun x then Just x else find fun xs
