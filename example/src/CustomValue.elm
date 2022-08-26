module CustomValue exposing (main)

import Autocomplete exposing (Autocomplete, choices)
import Autocomplete.View as AutocompleteView
import Browser
import Dict exposing (Dict)
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
    { autocompleteState : Autocomplete (Dict Int Dog) -- Int is Dog ID
    , selectedValue : Maybe ( Int, Dog )
    }


type alias Dog =
    { name : String
    , age : Int
    }


type Msg
    = OnAutocomplete (Autocomplete.Msg (Dict Int Dog))
    | OnAutocompleteSelect


fetcher : String -> Task Never (Autocomplete.Choices (Dict Int Dog))
fetcher query =
    let
        dogs =
            Dict.fromList
                [ ( 1, { name = "Hunter", age = 2 } )
                , ( 2, { name = "Polo", age = 3 } )
                , ( 3, { name = "Loki", age = 2 } )
                , ( 4, { name = "Angel", age = 4 } )
                , ( 5, { name = "Scout", age = 1 } )
                , ( 6, { name = "Lexi", age = 7 } )
                , ( 7, { name = "Zara", age = 8 } )
                , ( 8, { name = "Maya", age = 2 } )
                , ( 9, { name = "Baby", age = 9 } )
                , ( 10, { name = "Bud", age = 2 } )
                , ( 11, { name = "Ella", age = 3 } )
                , ( 12, { name = "Ace", age = 10 } )
                , ( 13, { name = "Kahlua", age = 1 } )
                , ( 14, { name = "Jake", age = 1 } )
                , ( 15, { name = "Apollo", age = 6 } )
                , ( 16, { name = "Sammy", age = 4 } )
                , ( 17, { name = "Puppy", age = 3 } )
                , ( 18, { name = "Gucci", age = 8 } )
                , ( 19, { name = "Mac", age = 1 } )
                , ( 20, { name = "Belle", age = 3 } )
                ]

        insensitiveStringContains : String -> Int -> Dog -> Bool
        insensitiveStringContains q _ dog =
            String.contains (String.toLower q) (String.toLower dog.name)

        choices : Dict Int Dog
        choices =
            if String.length query == 0 then
                Dict.empty

            else
                Dict.filter (insensitiveStringContains query) dogs
    in
    Task.succeed
        { query = query
        , choices = choices
        , length = List.length <| Dict.keys choices
        }



-- Model


init : ( Model, Cmd Msg )
init =
    ( { autocompleteState = Autocomplete.init { query = "", choices = Dict.empty, length = 0 } fetcher
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

                { choices, query, selectedIndex } =
                    Autocomplete.viewState autocompleteState

                choiceList =
                    Dict.toList choices

                selectedValue =
                    selectedIndex
                        |> Maybe.andThen (\i -> List.drop i choiceList |> List.head)
            in
            ( { model
                | selectedValue = selectedValue
                , autocompleteState =
                    Autocomplete.reset
                        { query = Maybe.withDefault query <| Maybe.map (Tuple.second >> .name) selectedValue
                        , choices = Dict.empty
                        , length = 0
                        }
                        model.autocompleteState
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        { selectedValue, autocompleteState } =
            model

        { query, choices, selectedIndex } =
            Autocomplete.viewState autocompleteState

        { inputEvents, choiceEvents } =
            AutocompleteView.events
                { onSelect = OnAutocompleteSelect
                , mapHtml = OnAutocomplete
                }
    in
    Html.div []
        [ Html.div []
            [ Html.text <|
                "Selected Value: "
                    ++ (Maybe.map toString selectedValue |> Maybe.withDefault "Nothing")
            ]
        , Html.input (inputEvents ++ [ Html.Attributes.value query ]) []
        , Html.div [] <|
            if Autocomplete.isFetching autocompleteState then
                [ Html.text "Fetching..." ]

            else if String.length query > 0 then
                Dict.toList choices
                    |> List.indexedMap (renderChoice choiceEvents selectedIndex)

            else
                [ Html.text "" ]
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> ( Int, Dog ) -> Html Msg
renderChoice events selectedIndex index ( id, dog ) =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text <| toString ( id, dog ) ]


toString : ( Int, Dog ) -> String
toString ( id, dog ) =
    dog.name ++ " #" ++ String.fromInt id
