module Board exposing (..)
import Piece exposing (..)
import File exposing (..)
import Rank exposing (..)

import Vector8 exposing (Vector8)
import Vector8 as Vector

import Vector4 exposing (Vector4)

import Vector2 exposing (Vector2)

type alias Board = 
    { tiles : Vector8 (Vector8 (Maybe Piece))

    }

on : File -> Rank -> Position
on = Position

get : Position -> Board -> Maybe Piece
get position board =
  board.tiles
    |> Vector.get (Rank.asIndex position.rank)
    |> Vector.get (File.asIndex position.file)

set : Position -> Maybe Piece -> Board -> Board
set position piece board = 
  let
    newRank =
      Vector.get (Rank.asIndex position.rank) board.tiles
      |> Vector.set (File.asIndex position.file) piece
  in
  
  { board | tiles = 
    board.tiles
      |> Vector.set (Rank.asIndex position.rank) newRank
  }

move : Position -> Position -> Board -> Board
move from to board =
  let
    piece = get from board
  in  
    board
        |> remove from 
        |> set to piece

remove : Position -> Board -> Board
remove position = set position Nothing

map : (Piece -> Maybe Piece) -> Board -> Board
map f board =
  { board | 
    tiles = board.tiles
            |> Vector.map (\file -> Vector.map (Maybe.andThen f) file)
  }

mapItem : Position -> (Piece -> Piece) -> Board -> Board
mapItem position f board =
  let
      oldItem = get position board
      newItem = Maybe.map f oldItem
  in
  set position newItem board
  
mapIndices : (Position-> Maybe Piece) -> Board -> Board
mapIndices f board =    
    { tiles =
      board.tiles
      |> Vector.indexedMap
        (\fileIndex file ->
          file
          |> Vector.indexedMap 
             (\rankIndex _ -> f <| Position (File.asFile fileIndex)(Rank.asRank rankIndex) )
        ) 
    } 

filterMaybes : List (Maybe a) -> List a
filterMaybes = List.filterMap identity

asList : Board -> List Piece
asList board =
  board.tiles
  |> Vector.foldl (\file acc -> Vector.toList file ++ acc) []  
  |> filterMaybes

asIndexedList : Board -> List (Position, Piece)
asIndexedList board =
  board.tiles
  |> Vector.indexedMap (\rankIndex rank -> (Rank.asRank rankIndex, rank))
  |> Vector.foldl (\(rank, file) acc ->
      (file
      |> Vector.toIndexedList
      |> List.filterMap (\(fileIndex, maybePiece) ->
        Maybe.map (\piece -> (Position (File.asFile fileIndex) rank, piece)) maybePiece
      )) ++ acc
     ) []  

emptyRank : Vector8 (Maybe Piece)
emptyRank = Vector8.repeat Nothing

emptyFile : Vector8 (Maybe Piece)
emptyFile = Vector8.repeat Nothing

emptyBoard : Board
emptyBoard = Vector.repeat Nothing |> Vector.repeat |> Board

startingBoard : Board
startingBoard =
  let
    firstRank = 
        Vector.from8 
        (Piece White Rook   |> Just)
        (Piece White Knight |> Just)
        (Piece White Bishop |> Just)
        (Piece White Queen  |> Just)
        (Piece White King   |> Just)
        (Piece White Bishop |> Just)
        (Piece White Knight |> Just)
        (Piece White Rook   |> Just)
    secondRank = 
        Vector.from8
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
        (Piece White Pawn |> Just)
    seventhRank =
        Vector8.from8
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
        (Piece Black Pawn |> Just)
    eighthRank =
        Vector8.from8
        (Piece Black Rook   |> Just)
        (Piece Black Knight |> Just)
        (Piece Black Bishop |> Just)
        (Piece Black Queen  |> Just)
        (Piece Black King   |> Just)
        (Piece Black Bishop |> Just)
        (Piece Black Knight |> Just)
        (Piece Black Rook   |> Just)
    tiles =
        Vector.from8
          firstRank
          secondRank
          emptyRank
          emptyRank
          emptyRank
          emptyRank
          seventhRank
          eighthRank
  in
    Board tiles

reduceMaybes : (Rank -> Maybe Rank) -> (File -> Maybe File) -> Maybe Rank -> Maybe File -> List Position
reduceMaybes f g maybeRank maybeFile =
  Maybe.map2
    (\rank file -> (Position file rank) :: reduceMaybes f g (f rank) (g file))
    maybeRank
    maybeFile

  |> Maybe.withDefault []

diagonals : Position -> Vector4 (List Position)
diagonals position =
  Vector4.from4
    (reduceMaybes Rank.succ File.succ (Rank.succ position.rank) (File.succ position.file))
    (reduceMaybes Rank.pred File.succ (Rank.pred position.rank) (File.succ position.file))
    (reduceMaybes Rank.pred File.pred (Rank.pred position.rank) (File.pred position.file))
    (reduceMaybes Rank.succ File.pred (Rank.succ position.rank) (File.pred position.file))

lines : Position -> Vector4 (List Position)
lines position =
  Vector4.from4
    (reduceMaybes Rank.succ                   (always Just position.file)  (Rank.succ position.rank) (Just      position.file))
    (reduceMaybes (always Just position.rank) File.succ                    (Just      position.rank) (File.succ position.file))
    (reduceMaybes Rank.pred                   (always Just position.file)  (Rank.pred position.rank) (Just      position.file))
    (reduceMaybes (always Just position.rank) File.pred                    (Just      position.rank) (File.pred position.file))

knightMoves : Position -> List Position
knightMoves position =
  let
      one : (a -> Maybe a) -> a -> Maybe a 
      one f = f
      two f = Maybe.andThen f << f

      fCombos = Vector2.from2 (one, two) (two, one)
      directions = Vector4.from4
                   (Rank.succ, File.succ)
                   (Rank.succ, File.pred)
                   (Rank.pred, File.succ)
                   (Rank.pred, File.pred)
      positions = multiply (\(a, b) (x, y) -> 
          Maybe.map2 (\rank file -> Position file rank) (a x position.rank) (b y position.file)
        ) fCombos directions
  in
    positions |> Vector8.toList |> filterMaybes

multiply : (a -> b -> c) -> Vector2 a -> Vector4 b -> Vector8 c
multiply f v2 v4 =
  Vector8.from8
    (f (Vector2.get Vector2.Index0 v2) (Vector4.get Vector4.Index0 v4))
    (f (Vector2.get Vector2.Index0 v2) (Vector4.get Vector4.Index1 v4))
    (f (Vector2.get Vector2.Index0 v2) (Vector4.get Vector4.Index2 v4))
    (f (Vector2.get Vector2.Index0 v2) (Vector4.get Vector4.Index3 v4))
    (f (Vector2.get Vector2.Index1 v2) (Vector4.get Vector4.Index0 v4))
    (f (Vector2.get Vector2.Index1 v2) (Vector4.get Vector4.Index1 v4))
    (f (Vector2.get Vector2.Index1 v2) (Vector4.get Vector4.Index2 v4))
    (f (Vector2.get Vector2.Index1 v2) (Vector4.get Vector4.Index3 v4))