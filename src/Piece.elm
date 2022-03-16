module Piece exposing (..)
import File exposing (File)
import Rank exposing (Rank)

type alias Position = { file : File, rank : Rank } 

type alias Piece = { color : Color
                   , pieceType : PieceType
                   }

type PieceType = Pawn | Rook | Knight | Bishop | Queen | King

type Color = White | Black

startingRank : Piece -> Rank
startingRank piece =
    case piece.pieceType of
        Pawn ->
            if piece.color == White then Rank.Two else Rank.Seven
        _ -> if piece.color == White then Rank.One else Rank.Eight

asCSSClass : Piece -> String
asCSSClass piece =
  let
      pieceColor =
        case piece.color of
          White -> "w"
          Black -> "b"
      pieceLetter =
        case piece.pieceType of
          Pawn   -> "p"
          Rook   -> "r"
          Knight -> "n"
          Bishop -> "b"
          Queen  -> "q"
          King   -> "k"

  in
    pieceColor ++ pieceLetter
