module Autocomplete exposing
    ( Autocomplete
    , Choices
    , Msg
    , ViewState
    , ViewStatus(..)
    , choices
    , init
    , isSelected
    , query
    , reset
    , selectedIndex
    , selectedValue
    , update
    , viewState
    )

import Debounce exposing (Debounce)
import Internal exposing (KeyDown(..), Msg(..))
import Task exposing (Task)


type alias Msg a =
    Internal.Msg a


type alias Choices a =
    Internal.Choices a


type Autocomplete a
    = Autocomplete (State a)


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


type alias ViewState a =
    { query : String
    , choices : List a
    , ignoreList : List a
    , selectedIndex : Maybe Int
    , status : ViewStatus
    }


type ViewStatus
    = NotFetched
    | Fetching
    | Error String
    | FetchedChoices


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


reset : Choices a -> Autocomplete a -> Autocomplete a
reset c (Autocomplete s) =
    init c s.fetcher


selectedValue : Autocomplete a -> Maybe a
selectedValue (Autocomplete s) =
    s.selectedIndex
        |> Maybe.map (\i -> List.drop i s.choices)
        |> Maybe.andThen List.head



-- Accessors


viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , ignoreList = s.ignoreList
    , selectedIndex = s.selectedIndex
    , status = s.viewStatus
    }


query : Autocomplete a -> String
query (Autocomplete s) =
    s.query


choices : Autocomplete a -> List a
choices (Autocomplete s) =
    s.choices


selectedIndex : Autocomplete a -> Maybe Int
selectedIndex (Autocomplete s) =
    s.selectedIndex


isSelected : Maybe Int -> Int -> Bool
isSelected selected index =
    case selected of
        Nothing ->
            False

        Just i ->
            i == index
