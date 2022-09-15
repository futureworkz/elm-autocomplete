module Autocomplete.View exposing
    ( Events, EventMapper
    , events
    )

{-| Autocomplete.View exposes HTML events to be attached for input and every autocomplete choice.


# Type

@docs Events, EventMapper


# Attributes

@docs events

-}

import Html exposing (Attribute)
import Html.Events as Events
import Internal exposing (KeyDown(..), Msg(..))
import Json.Decode as JD


{-| Record to hold the events to be attached for input and every autocomplete choice
-}
type alias Events msg =
    { inputEvents : List (Attribute msg)
    , choiceEvents : Int -> List (Attribute msg)
    }


{-| Map Autocomplete Msg into your app's msg and also the msg to send when user selects a choice
-}
type alias EventMapper a msg =
    { onSelect : msg
    , mapHtml : Msg a -> msg
    }


{-| Returns the events to be attached for input and every autocomplete choice
-}
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
    , Events.preventDefaultOn "keydown" <| onKeyDownDecoder mapper
    ]


{-| Clicking on the choices has a few scenarios we need to consider:

1.  Send a message to Autocomplete to set selectedIndex and then a message to user for processing (eg. set selectedValue)
2.  Close the choices popout _conditionally_ via onBlur event on the popup

-}
choiceEvents : EventMapper a msg -> Int -> List (Attribute msg)
choiceEvents mapper index =
    let
        { onSelect, mapHtml } =
            mapper
    in
    [ -- onClick event is used to send msg to user
      Events.onClick onSelect

    -- onMouseDown and onMouseUp is sent to Autocomplete to detect a full click and update the selectedIndex
    -- preventDefault onMouseDown is to prevent firing onBlur on the input (if implemented by user)
    , Events.preventDefaultOn "mousedown" <| JD.succeed ( mapHtml <| OnMouseDown index, True )
    , Events.onMouseUp <| mapHtml <| OnMouseUp index
    ]


onKeyDownDecoder : EventMapper a msg -> JD.Decoder ( msg, Bool )
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
                        JD.succeed ( mapHtml <| OnKeyDown ArrowUp, True )

                    "ArrowDown" ->
                        JD.succeed ( mapHtml <| OnKeyDown ArrowDown, True )

                    "Enter" ->
                        JD.succeed ( onSelect, True )

                    _ ->
                        JD.fail "Ignore other keys"
            )
