module Lazy.List.Extra
    exposing
        ( compute
        , drop
        , scanState
        , scanl
        )

import Lazy exposing (force, lazy)
import Lazy.List exposing (LazyList, LazyListView(..), takeWhile)


scanl : (a -> b -> b) -> b -> LazyList a -> LazyList b
scanl foldFunction initialValue list =
    lazy <|
        \() ->
            case force list of
                Cons a rest ->
                    let
                        newValue =
                            foldFunction a initialValue
                    in
                    Cons newValue <| scanl foldFunction newValue rest

                Nil ->
                    Nil


scanState : (member -> state -> ( result, state )) -> state -> LazyList member -> LazyList result
scanState foldFunction initialState list =
    lazy <|
        \() ->
            case force list of
                Cons a rest ->
                    let
                        ( newValue, newState ) =
                            foldFunction a initialState
                    in
                    Cons newValue <|
                        scanState foldFunction newState rest

                Nil ->
                    Nil


drop : Int -> LazyList a -> LazyList a
drop number list =
    let
        dropHelper number list =
            case force list of
                Cons a rest ->
                    if number <= 0 then
                        Cons a rest
                    else
                        dropHelper (number - 1) rest

                Nil ->
                    Nil
    in
    lazy <|
        \() ->
            dropHelper number list


compute : (a -> a) -> a -> LazyList a
compute iterationFunction seed =
    lazy <|
        \() ->
            let
                newValue =
                    iterationFunction seed
            in
            Cons seed <| compute iterationFunction newValue
