# Elm Autocomplete

Elm Autocomplete with async and debouncing.

This package provides the core autocomplete logic and state but does *NOT* provide any view functions to display the autocomplete. 

It is the design philosophy of this package to allow users to create and style autocomplete input as needed without concerns on how to manage the logic.

# Simple Example

```
module main exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Attribute, Html)
import Html.Attributes
import Task exposing (Task)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { autocompleteState : Autocomplete String
    , selectedValue : Maybe String
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg String)
    | OnAutocompleteSelect


fetcher : Autocomplete.Choices String -> Task String (Autocomplete.Choices String)
fetcher lastChoices =
    let
        dogs =
            [ "Hunter"
            , "Polo"
            , "Loki"
            , "Angel"
            , "Scout"
            , "Lexi"
            , "Zara"
            , "Maya"
            , "Baby"
            , "Bud"
            , "Ella"
            , "Ace"
            , "Kahlua"
            , "Jake"
            , "Apollo"
            , "Sammy"
            , "Puppy"
            , "Gucci"
            , "Mac"
            , "Belle"
            ]

        insensitiveStringContains : String -> String -> Bool
        insensitiveStringContains a b =
            String.contains (String.toLower a) (String.toLower b)

        choiceList : List String
        choiceList =
            if String.length lastChoices.query == 0 then
                []

            else
                List.filter (insensitiveStringContains lastChoices.query) dogs
    in
    Task.succeed { lastChoices | choices = choiceList }



-- Model


init : ( Model, Cmd Msg )
init =
    ( { autocompleteState = Autocomplete.init { query = "", choices = [], ignoreList = [] } fetcher
      , selectedValue = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAutocomplete autocompleteMsg ->
            let
                ( newAutocompleteState, autoCompleteCmd ) =
                    Autocomplete.update autocompleteMsg model.autocompleteState
            in
            ( { model | autocompleteState = newAutocompleteState }
            , Cmd.map OnAutocomplete autoCompleteCmd
            )

        OnAutocompleteSelect ->
            let
                { autocompleteState } =
                    model

                query =
                    Autocomplete.query autocompleteState

                selectedValue =
                    Autocomplete.selectedValue autocompleteState
            in
            ( { model
                | selectedValue = selectedValue
                , autocompleteState =
                    Autocomplete.reset
                        { query = Maybe.withDefault query selectedValue
                        , choices = []
                        , ignoreList = []
                        }
                        autocompleteState
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        { selectedValue, autocompleteState } =
            model

        { query, choices, selectedIndex, status } =
            Autocomplete.viewState autocompleteState

        { inputEvents, choiceEvents } =
            AutocompleteView.events
                { onSelect = OnAutocompleteSelect
                , mapHtml = OnAutocomplete
                }
    in
    Html.div []
        [ Html.div [] [ Html.text <| "Selected Value: " ++ Maybe.withDefault "Nothing" selectedValue ]
        , Html.input (inputEvents ++ [ Html.Attributes.value query ]) []
        , Html.div [] <|
            case status of
                Autocomplete.NotFetched ->
                    [ Html.text "" ]

                Autocomplete.Fetching ->
                    [ Html.text "Fetching..." ]

                Autocomplete.Error s ->
                    [ Html.text s ]

                Autocomplete.FetchedChoices ->
                    if String.length query > 0 then
                        List.indexedMap (renderChoice choiceEvents selectedIndex) choices

                    else
                        [ Html.text "" ]
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> String -> Html Msg
renderChoice events selectedIndex index s =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text s ]
```

# Try it out

Navigate to `/examples` and run `elm reactor` to play with the example.

# TODO
- Add `elm-css` view
- Add code examples in README
- Add build/git hooks/etc
