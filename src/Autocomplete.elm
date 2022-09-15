module Autocomplete exposing
    ( Autocomplete, Msg, Choices, ViewState, ViewStatus(..)
    , init, update
    , reset, selectedValue
    , viewState, query, choices, selectedIndex, isSelected
    )

{-| Autocomplete contains the main logic to handle auto-complete.
The state and logic of Autocomplete should reside within your model.
Please refer to our examples to see how it all linked up.

To render the Autocomplete, please refer to `Autocomplete.View` or `Autocomplete.Styled`.


# Types

@docs Autocomplete, Msg, Choices, ViewState, ViewStatus


# State Management

@docs init, update


# Helpers

@docs reset, selectedValue


# Accessors

@docs viewState, query, choices, selectedIndex, isSelected

-}

import Debounce exposing (Debounce)
import Internal exposing (KeyDown(..), Msg(..))
import Task exposing (Task)


{-| Opaque type of Autocomplete state
-}
type Autocomplete a
    = Autocomplete (State a)


{-| Opaque type of Autocomplete internal msg
-}
type alias Msg a =
    Internal.Msg a


{-| Record to hold the query and choices for your fetcher function.

    type alias Choices a =
        { query : String
        , choices : List a
        , ignoreList : List a
        }

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

-}
type alias Choices a =
    Internal.Choices a


type alias State a =
    { query : String
    , choices : List a
    , ignoreList : List a
    , fetcher : Choices a -> Task String (Choices a)
    , viewStatus : ViewStatus
    , selectedIndex : Maybe Int
    , mouseDownIndex : Maybe Int
    , debounceConfig : Debounce.Config (Msg a)
    , debounceState : Debounce String
    }


{-| Record to expose common values of Autocomplete to be used for display
-}
type alias ViewState a =
    { query : String
    , choices : List a
    , ignoreList : List a
    , selectedIndex : Maybe Int
    , status : ViewStatus
    }


{-| A useful union type for rendering the correct view for each state of Autocomplete
-}
type ViewStatus
    = NotFetched
    | Fetching
    | Error String
    | FetchedChoices


{-| Initialize the Autocomplete state with your fetcher function

    init : ( Model, Cmd Msg )
    init =
        ( { -- Initialize the Autocomplete state
            autocompleteState = Autocomplete.init { query = "", choices = [], ignoreList = [] } fetcher
          , selectedValue = Nothing
          }
        , Cmd.none
        )

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

-}
init : Choices a -> (Choices a -> Task String (Choices a)) -> Autocomplete a
init initChoices fetcher =
    Autocomplete
        { query = initChoices.query
        , choices = initChoices.choices
        , ignoreList = initChoices.ignoreList
        , fetcher = fetcher
        , viewStatus = NotFetched
        , selectedIndex = Nothing
        , mouseDownIndex = Nothing
        , debounceConfig =
            { strategy = Debounce.later 200
            , transform = DebounceMsg
            }
        , debounceState = Debounce.init
        }


{-| Updates the Autocomplete state


    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            -- This is the main wire-up to pass Autocomplete Msg to Autocomplete state
            OnAutocomplete autocompleteMsg ->
                let
                    ( newAutocompleteState, autoCompleteCmd ) =
                        Autocomplete.update autocompleteMsg model.autocompleteState
                in
                ( { model | autocompleteState = newAutocompleteState }
                , Cmd.map OnAutocomplete autoCompleteCmd
                )

    -- ...

-}
update : Msg a -> Autocomplete a -> ( Autocomplete a, Cmd (Msg a) )
update msg (Autocomplete state) =
    case msg of
        OnInput q ->
            let
                ( debounceState, debounceCmd ) =
                    Debounce.push state.debounceConfig q state.debounceState
            in
            ( Autocomplete
                { state
                    | query = q
                    , viewStatus = Fetching
                    , selectedIndex = Nothing
                    , debounceState = debounceState
                }
            , debounceCmd
            )

        DebounceMsg debouceMsg ->
            let
                doFetchCmd : String -> Cmd (Msg a)
                doFetchCmd s =
                    Task.perform DoFetch <| Task.succeed { query = s, choices = state.choices, ignoreList = state.ignoreList }

                ( debounceState, debounceCmd ) =
                    Debounce.update
                        state.debounceConfig
                        (Debounce.takeLast doFetchCmd)
                        debouceMsg
                        state.debounceState
            in
            ( Autocomplete { state | debounceState = debounceState }, debounceCmd )

        DoFetch c ->
            ( Autocomplete state, Task.attempt OnFetch <| state.fetcher c )

        OnFetch result ->
            case result of
                Err s ->
                    ( Autocomplete { state | viewStatus = Error s }, Cmd.none )

                Ok c ->
                    if c.query == state.query then
                        ( Autocomplete
                            { state
                                | choices =
                                    List.filter
                                        (\i -> not <| List.member i c.ignoreList)
                                        c.choices
                                , ignoreList = c.ignoreList
                                , viewStatus = FetchedChoices
                            }
                        , Cmd.none
                        )

                    else
                        ( Autocomplete state, Cmd.none )

        OnKeyDown keyDown ->
            ( Autocomplete
                { state
                    | selectedIndex =
                        Internal.calculateIndex
                            (List.length state.choices)
                            state.selectedIndex
                            keyDown
                }
            , Cmd.none
            )

        OnMouseDown index ->
            ( Autocomplete { state | mouseDownIndex = Just index }, Cmd.none )

        OnMouseUp upIndex ->
            -- Check that mouse down and up have the same index
            -- otherwise ignore this event
            if state.mouseDownIndex == Just upIndex then
                ( Autocomplete
                    { state
                        | selectedIndex = Just upIndex
                        , mouseDownIndex = Nothing
                    }
                , Cmd.none
                )

            else
                ( Autocomplete state, Cmd.none )



-- Helpers


{-| Reset the Autocomplete State

There are many scenarios Autocomplete can handle using the `reset`.

On selected value, display selectedValue but remove all choices:

    Autocomplete.reset
        { query = Maybe.withDefault query selectedValue
        , choices = []
        , ignoreList = []
        }
        autocompleteState

On selected multiple values, ignore selected values but still display the choices:

    Autocomplete.reset
        { query = ""
        , choices = Autocomplete.choices autocompleteState
        , ignoreList = selectedValueList
        }
        autocompleteState

-}
reset : Choices a -> Autocomplete a -> Autocomplete a
reset c (Autocomplete s) =
    init c s.fetcher


{-| Returns the selectedValue
-}
selectedValue : Autocomplete a -> Maybe a
selectedValue (Autocomplete s) =
    s.selectedIndex
        |> Maybe.map (\i -> List.drop i s.choices)
        |> Maybe.andThen List.head



-- Accessors


{-| Returns the ViewState of the Autocomplete to render the view.
Remember to attach Autocomplete events to your view!
See [Autocomplete.View](Autocomplete.View#events)
-}
viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , ignoreList = s.ignoreList
    , selectedIndex = s.selectedIndex
    , status = s.viewStatus
    }


{-| Returns the query of the Autocomplete
-}
query : Autocomplete a -> String
query (Autocomplete s) =
    s.query


{-| Returns the current list of choices
-}
choices : Autocomplete a -> List a
choices (Autocomplete s) =
    s.choices


{-| Returns the selected index of the Autocomplete
-}
selectedIndex : Autocomplete a -> Maybe Int
selectedIndex (Autocomplete s) =
    s.selectedIndex


{-| Helper function to calculate if an index is selected

    renderChoice : (Int -> List (Attribute Msg)) -> Maybe Int -> Int -> String -> Html Msg
    renderChoice events selectedIndex index s =
        Html.div
            (if Autocomplete.isSelected selectedIndex index then
                Html.Attributes.style "backgroundColor" "#EEE" :: events index

             else
                Html.Attributes.style "backgroundColor" "#FFF" :: events index
            )
            [ Html.text s ]

-}
isSelected : Maybe Int -> Int -> Bool
isSelected selected index =
    case selected of
        Nothing ->
            False

        Just i ->
            i == index
