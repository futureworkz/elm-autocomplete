module CustomValue exposing (main)

import Autocomplete exposing (Autocomplete, choices)
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
    { autocompleteState : Autocomplete Animal
    , selectedValue : Maybe Animal
    }


type Animal
    = Dog String
    | Cat String
    | Fish String


type Msg
    = OnAutocomplete (Autocomplete.Msg Animal)
    | OnAutocompleteSelect


fetcher : String -> Task Never (Autocomplete.Choices Animal)
fetcher query =
    let
        animals =
            [ Dog "Hunter"
            , Cat "Polo"
            , Fish "Loki"
            , Dog "Angel"
            , Cat "Scout"
            , Fish "Lexi"
            , Dog "Zara"
            , Cat "Maya"
            , Fish "Baby"
            , Dog "Bud"
            , Cat "Ella"
            , Fish "Ace"
            , Dog "Kahlua"
            , Cat "Jake"
            , Fish "Apollo"
            , Dog "Sammy"
            , Cat "Puppy"
            , Fish "Gucci"
            , Dog "Mac"
            , Cat "Belle"
            ]

        insensitiveStringContains : String -> Animal -> Bool
        insensitiveStringContains q animal =
            String.contains (String.toLower q) (String.toLower <| getName animal)

        choices : List Animal
        choices =
            if String.length query == 0 then
                []

            else
                List.filter (insensitiveStringContains query) animals
    in
    Task.succeed
        { query = query
        , choices = choices
        }



-- Model


init : ( Model, Cmd Msg )
init =
    ( { autocompleteState = Autocomplete.init { query = "", choices = [] } fetcher
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

                selectedValue =
                    selectedIndex
                        |> Maybe.andThen (\i -> List.drop i choices |> List.head)

                resetedAutocompleteState =
                    Autocomplete.reset
                        { query = Maybe.withDefault query <| Maybe.map getName selectedValue
                        , choices = []
                        }
                        autocompleteState
            in
            ( { model
                | selectedValue = selectedValue
                , autocompleteState =
                    resetedAutocompleteState
                        |> Autocomplete.setIgnoreList (Maybe.withDefault [] <| Maybe.map List.singleton selectedValue)
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
                    ++ (Maybe.map getAnimalLabel selectedValue |> Maybe.withDefault "Nothing")
            ]
        , Html.input (inputEvents ++ [ Html.Attributes.value query ]) []
        , Html.div [] <|
            if Autocomplete.isFetching autocompleteState then
                [ Html.text "Fetching..." ]

            else if String.length query > 0 then
                List.indexedMap (renderChoice choiceEvents selectedIndex) choices

            else
                [ Html.text "" ]
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> Animal -> Html Msg
renderChoice events selectedIndex index animal =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text <| getAnimalLabel animal ]



-- Helper


getName : Animal -> String
getName animal =
    case animal of
        Dog name ->
            name

        Cat name ->
            name

        Fish name ->
            name


getAnimalLabel : Animal -> String
getAnimalLabel animal =
    case animal of
        Dog name ->
            "Dog: " ++ name

        Cat name ->
            "Cat: " ++ name

        Fish name ->
            "Fish: " ++ name
