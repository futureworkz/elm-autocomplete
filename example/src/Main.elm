module Main exposing (main)

import Autocomplete exposing (Autocomplete)
import Autocomplete.View as AutocompleteView
import Browser
import Html exposing (Html)
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
    { autocompleteState : Autocomplete
    }


type Msg
    = OnAutocomplete Autocomplete.Msg


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


view : Model -> Html Msg
view model =
    Html.map OnAutocomplete <|
        Html.div []
            [ AutocompleteView.input model.autocompleteState
            , AutocompleteView.suggestions model.autocompleteState
            ]
