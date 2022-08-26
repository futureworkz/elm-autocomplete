module Autocomplete.View exposing (events)

import Html exposing (Attribute)
import Html.Events as Events
import Internal exposing (KeyDown(..), Msg(..))
import Json.Decode as JD


type alias Events msg =
    { inputEvents : List (Attribute msg)
    , choiceEvents : Int -> List (Attribute msg)
    }


type alias EventMapper a msg =
    { onSelect : msg
    , mapHtml : Msg a -> msg
    }


events : EventMapper a msg -> Events msg
events mapper =
    { inputEvents = inputEvents mapper
    , choiceEvents = choiceEvents mapper
    }


inputEvents : EventMapper a msg -> List (Attribute msg)
inputEvents mapper =
    let
        { mapHtml } =
            mapper
    in
    [ Events.onInput (mapHtml << OnInput)
    , Events.on "keydown" <| onKeyDownDecoder mapper
    ]


choiceEvents : EventMapper a msg -> Int -> List (Attribute msg)
choiceEvents mapper index =
    let
        { onSelect, mapHtml } =
            mapper
    in
    -- We cannot use onClick twice
    -- so we use onMouseDown/Up to send to Autocomplete first
    -- and then onClick will send to user's app
    -- onMouseDown/Up will always fire before onClick
    -- See https://www.w3schools.com/jsref/event_onmouseup.asp
    [ Events.onMouseDown <| mapHtml <| OnMouseDown index
    , Events.onMouseUp <| mapHtml <| OnMouseUp index
    , Events.onClick onSelect
    ]


onKeyDownDecoder : EventMapper a msg -> JD.Decoder msg
onKeyDownDecoder mapper =
    let
        { onSelect, mapHtml } =
            mapper
    in
    JD.field "key" JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        JD.succeed <| mapHtml <| OnKeyDown ArrowUp

                    "ArrowDown" ->
                        JD.succeed <| mapHtml <| OnKeyDown ArrowDown

                    "Enter" ->
                        JD.succeed onSelect

                    _ ->
                        JD.fail "Ignore other keys"
            )
