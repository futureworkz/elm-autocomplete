module Autocomplete.View exposing
    ( input
    , inputAttributes
    , suggestions
    )

import Autocomplete exposing (Autocomplete)
import Html exposing (Attribute, Html, div, text)
import Html.Events as Events
import Internal exposing (KeyDown(..), Msg(..))
import Json.Decode as JD


input : Autocomplete -> Html Msg
input _ =
    Html.input inputAttributes []


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


inputAttributes : List (Attribute Msg)
inputAttributes =
    [ Events.onInput OnInput
    , Events.on "keydown" <| JD.map OnKeyDown keyDownDecoder
    ]


keyDownDecoder : JD.Decoder KeyDown
keyDownDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        JD.succeed ArrowUp

                    "ArrowDown" ->
                        JD.succeed ArrowDown

                    _ ->
                        JD.fail "Ignore other keys"
            )
