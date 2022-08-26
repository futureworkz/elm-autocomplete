module Main exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Html, a)
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
    { autocompleteState : Autocomplete (List String)
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg (List String))


fetcher : String -> Task Never (Autocomplete.Choices (List String))
fetcher query =
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

        choices : List String
        choices =
            if String.length query == 0 then
                []

            else
                List.filter (insensitiveStringContains query) dogs
    in
    Task.succeed { choices = choices, length = List.length choices }



-- Model


init : ( Model, Cmd Msg )
init =
    ( { autocompleteState = Autocomplete.init { choices = [], length = 0 } fetcher
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



-- View


view : Model -> Html Msg
view model =
    let
        { autocompleteState } =
            model

        { query, choices, selectedIndex } =
            Autocomplete.viewState autocompleteState
    in
    Html.div []
        [ Html.input (AutocompleteView.events OnAutocomplete ++ [ Html.Attributes.value query ]) []
        , Html.div [] <|
            if Autocomplete.isFetching autocompleteState then
                [ Html.text "Fetching..." ]

            else if String.length query > 0 then
                List.indexedMap (renderChoice selectedIndex) choices

            else
                [ Html.text "" ]
        ]


renderChoice : Maybe Int -> Int -> String -> Html Msg
renderChoice selectedIndex index s =
    Html.div
        [ if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE"

          else
            Html.Attributes.style "backgroundColor" "#FFF"
        ]
        [ Html.text s ]
