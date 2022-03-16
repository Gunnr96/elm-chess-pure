module File exposing (..)

import Vector8 exposing (..)

type File = A | B | C | D | E | F | G | H


succ : File -> Maybe File
succ file =
  case file of
    A -> Just B
    B -> Just C
    C -> Just D
    D -> Just E
    E -> Just F
    F -> Just G
    G -> Just H
    H -> Nothing

pred : File -> Maybe File
pred file =
  case file of
    A -> Nothing
    B -> Just A
    C -> Just B
    D -> Just C
    E -> Just D
    F -> Just E
    G -> Just F
    H -> Just G

asIndex : File -> Index
asIndex file =
  case file of
    A -> Index0
    B -> Index1
    C -> Index2
    D -> Index3
    E -> Index4
    F -> Index5
    G -> Index6
    H -> Index7

asFile : Index -> File
asFile index =
  case index of
    Index0 -> A 
    Index1 -> B 
    Index2 -> C 
    Index3 -> D 
    Index4 -> E 
    Index5 -> F 
    Index6 -> G 
    Index7 -> H 

asInt : File -> Int
asInt file =
  case file of
    A -> 1
    B -> 2
    C -> 3
    D -> 4
    E -> 5
    F -> 6
    G -> 7
    H -> 8

asString : File -> String
asString file =
  case file of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"
    F -> "F"
    G -> "G"
    H -> "H"