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


fetcher : Autocomplete.Choices Animal -> Task String (Autocomplete.Choices Animal)
fetcher lastChoices =
    if String.length lastChoices.query < 2 then
        -- Simple validation
        Task.fail "Enter at least 2 characters to search"

    else
        -- Passed validation, fetch data
        -- Return error if fetched data is empty
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
                String.contains (String.toLower q) (String.toLower <| animalName animal)

            choiceList : List Animal
            choiceList =
                List.filter (insensitiveStringContains lastChoices.query) animals
        in
        if List.isEmpty choiceList then
            Task.fail "Error: Animal not found!"

        else
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
                        { query = Maybe.withDefault query <| Maybe.map animalName selectedValue
                        , choices = []
                        , ignoreList = Maybe.withDefault [] <| Maybe.map List.singleton selectedValue
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
        [ Html.div []
            [ Html.text <|
                "Selected Value: "
                    ++ (Maybe.map animalLabel selectedValue |> Maybe.withDefault "Nothing")
            ]
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
                    List.indexedMap (renderChoice choiceEvents selectedIndex) choices
        ]


renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> Animal -> Html Msg
renderChoice events selectedIndex index animal =
    Html.div
        (if Autocomplete.isSelected selectedIndex index then
            Html.Attributes.style "backgroundColor" "#EEE" :: events index

         else
            Html.Attributes.style "backgroundColor" "#FFF" :: events index
        )
        [ Html.text <| animalLabel animal ]



-- Helper


animalName : Animal -> String
animalName animal =
    case animal of
        Dog name ->
            name

        Cat name ->
            name

        Fish name ->
            name


animalLabel : Animal -> String
animalLabel animal =
    case animal of
        Dog name ->
            "Dog: " ++ name

        Cat name ->
            "Cat: " ++ name

        Fish name ->
            "Fish: " ++ name
