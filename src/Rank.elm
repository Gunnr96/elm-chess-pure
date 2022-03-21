module Rank exposing (..)

import Vector8 exposing (..)
import MaybeExtra exposing (kleisliArrow)

type Rank = One | Two | Three | Four | Five | Six | Seven | Eight

succ : Rank -> Maybe Rank
succ rank =
  case rank of
    One   -> Just Two
    Two   -> Just Three
    Three -> Just Four
    Four  -> Just Five  
    Five  -> Just Six   
    Six   -> Just Seven 
    Seven -> Just Eight 
    Eight -> Nothing

pred : Rank -> Maybe Rank
pred rank =
  case rank of
    One   -> Nothing
    Two   -> Just One
    Three -> Just Two 
    Four  -> Just Three
    Five  -> Just Four
    Six   -> Just Five 
    Seven -> Just Six  
    Eight -> Just Seven

asIndex : Rank -> Index
asIndex rank =
  case rank of
    One    -> Index0
    Two    -> Index1
    Three  -> Index2 
    Four   -> Index3
    Five   -> Index4
    Six    -> Index5
    Seven  -> Index6 
    Eight  -> Index7

asRank : Index -> Rank
asRank index =
  case index of
    Index0  -> One
    Index1  -> Two
    Index2  -> Three
    Index3  -> Four
    Index4  -> Five
    Index5  -> Six
    Index6  -> Seven
    Index7  -> Eight


asInt : Rank -> Int
asInt rank =
  case rank of
    One    -> 1
    Two    -> 2
    Three  -> 3 
    Four   -> 4
    Five   -> 5
    Six    -> 6
    Seven  -> 7 
    Eight  -> 8

offset : Int -> Rank -> Maybe Rank
offset n =
  let 
    f = if n > 0 then succ else pred
  in
  List.repeat (abs n) f
  |> List.foldr kleisliArrow Just

asString : Rank -> String
asString = String.fromInt << asInt