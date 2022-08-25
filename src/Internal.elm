module Internal exposing (KeyDown(..), Msg(..), calculateIndex)

import Debounce



-- This file is not exposed as a module


type Msg
    = OnInput String
    | DoFetch String
    | OnFetch (Result String (List String))
    | OnKeyDown KeyDown
    | DebounceMsg Debounce.Msg


type KeyDown
    = ArrowUp
    | ArrowDown


calculateIndex : List String -> Maybe Int -> KeyDown -> Maybe Int
calculateIndex xs selectedIndex keyDown =
    case xs of
        [] ->
            Nothing

        _ ->
            let
                len =
                    List.length xs
            in
            case ( selectedIndex, keyDown ) of
                ( Nothing, ArrowUp ) ->
                    Just <| len - 1

                ( Nothing, ArrowDown ) ->
                    Just 0

                ( Just i, ArrowUp ) ->
                    Just <| wrapAround len (i - 1)

                ( Just i, ArrowDown ) ->
                    Just <| wrapAround len (i + 1)


wrapAround : Int -> Int -> Int
wrapAround len index =
    if index < 0 then
        len - 1

    else if index >= len then
        0

    else
        index
