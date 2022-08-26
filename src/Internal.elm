module Internal exposing (KeyDown(..), Msg(..), calculateIndex)

import Debounce



-- This file is not exposed as a module


type Msg e a
    = OnInput String
    | DoFetch String
    | OnFetch (Result e (List a))
    | OnKeyDown KeyDown
    | DebounceMsg Debounce.Msg


type KeyDown
    = ArrowUp
    | ArrowDown
    | Enter


calculateIndex : List a -> Maybe Int -> KeyDown -> Maybe Int
calculateIndex xs currentIndex keyDown =
    case xs of
        [] ->
            Nothing

        _ ->
            let
                len =
                    List.length xs
            in
            case ( currentIndex, keyDown ) of
                ( Nothing, ArrowUp ) ->
                    Just <| len - 1

                ( Nothing, ArrowDown ) ->
                    Just 0

                ( Just i, ArrowUp ) ->
                    Just <| wrapAround len (i - 1)

                ( Just i, ArrowDown ) ->
                    Just <| wrapAround len (i + 1)

                ( _, Enter ) ->
                    currentIndex


wrapAround : Int -> Int -> Int
wrapAround len index =
    if index < 0 then
        len - 1

    else if index >= len then
        0

    else
        index
