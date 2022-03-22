module MaybeExtra exposing (..)

catMaybes : List (Maybe a) -> List a
catMaybes list =
  case list of
    [] -> []
    (x::xs) ->
        case x of
            Nothing -> catMaybes xs
            Just x1 -> x1 :: catMaybes xs

maybeToList : Maybe a -> List a
maybeToList x =
  case x of
    Just x1 -> [x1]
    Nothing -> []
    
kleisliArrow : (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c
kleisliArrow f g = \x -> f x |> Maybe.andThen g