module Autocomplete.View exposing (events)

import Html exposing (Attribute)
import Html.Attributes
import Html.Events as Events
import Internal exposing (KeyDown(..), Msg(..))
import Json.Decode as JD


events : (Msg e a -> msg) -> List (Attribute msg)
events tagger =
    List.map (Html.Attributes.map tagger)
        [ Events.onInput OnInput
        , Events.on "keydown" <| JD.map OnKeyDown arrowKeyDecoder
        ]


arrowKeyDecoder : JD.Decoder KeyDown
arrowKeyDecoder =
    JD.field "key" JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        JD.succeed ArrowUp

                    "ArrowDown" ->
                        JD.succeed ArrowDown

                    "Enter" ->
                        JD.succeed Enter

                    _ ->
                        JD.fail "Ignore other keys"
            )
