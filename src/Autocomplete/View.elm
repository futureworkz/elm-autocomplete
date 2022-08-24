module Autocomplete.View exposing (input, suggestions)

import Autocomplete exposing (Autocomplete)
import Html exposing (Html, div, text)
import Html.Events as Events
import Type exposing (Msg(..))


input : Autocomplete -> Html Msg
input _ =
    Html.input
        [ Events.onInput OnInput
        ]
        []


suggestions : Autocomplete -> Html Msg
suggestions autocomplete =
    case Autocomplete.suggestions autocomplete of
        Err e ->
            div [] [ text e ]

        Ok s ->
            div [] <| List.map suggestion s


suggestion : String -> Html Msg
suggestion s =
    div [] [ text s ]
