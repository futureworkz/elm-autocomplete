module Internal exposing (Choices, KeyDown(..), Msg(..), calculateIndex)

import Debounce



-- This file is not exposed as a module


type Msg a
    = OnInput String
    | DoFetch (Choices a)
    | OnFetch (Result String (Choices a))
    | OnKeyDown KeyDown
    | OnMouseDown Int
    | OnMouseUp Int
    | DebounceMsg Debounce.Msg


type alias Choices a =
    { query : String
    , choices : List a
    , ignoreList : List a
    }


type KeyDown
    = ArrowUp
    | ArrowDown
    | Enter


calculateIndex : Int -> Maybe Int -> KeyDown -> Maybe Int
calculateIndex len currentIndex keyDown =
    if len <= 0 then
        Nothing

    else
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
