module Main exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Html)
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
    { autocompleteState : Autocomplete String String
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg String String)


fetcher : String -> Task String (List String)
fetcher s =
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
    in
    case List.filter (insensitiveStringContains s) dogs of
        [] ->
            Task.fail "No dog name found."

        xs ->
            Task.succeed xs



-- Model


init : ( Model, Cmd Msg )
init =
    ( { autocompleteState = Autocomplete.init fetcher
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

        query =
            Autocomplete.query autocompleteState

        suggestions =
            Autocomplete.suggestions autocompleteState

        selectedIndex =
            Autocomplete.selectedIndex autocompleteState
    in
    Html.div []
        [ Html.input (AutocompleteView.events OnAutocomplete ++ [ Html.Attributes.value query ]) []
        , case suggestions of
            Err e ->
                Html.div [] [ Html.text e ]

            Ok s ->
                Html.div [] <| List.indexedMap (suggestion selectedIndex) s
        ]


suggestion : Maybe Int -> Int -> String -> Html Msg
suggestion selectedIndex index s =
    let
        isSelected =
            selectedIndex
                |> Maybe.map (\i -> i == index)
                |> Maybe.withDefault False
    in
    Html.div
        [ if isSelected then
            Html.Attributes.style "backgroundColor" "#EEE"

          else
            Html.Attributes.style "backgroundColor" "#FFF"
        ]
        [ Html.text s ]
