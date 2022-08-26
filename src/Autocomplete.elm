module Autocomplete exposing
    ( Autocomplete
    , Choices
    , Msg
    , ViewState
    , choices
    , init
    , isFetching
    , isSelected
    , query
    , reset
    , selectedIndex
    , setIgnoreList
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
    , selectedIndex : Maybe Int
    , fetcher : String -> Task Never (Choices a)
    , isFetching : Bool
    , mouseDownIndex : Maybe Int
    , debounceConfig : Debounce.Config (Msg a)
    , debounceState : Debounce String
    }


type alias ViewState a =
    { query : String
    , choices : List a
    , selectedIndex : Maybe Int
    , isFetching : Bool
    }


init : Choices a -> (String -> Task Never (Choices a)) -> Autocomplete a
init initChoices fetcher =
    Autocomplete
        { query = initChoices.query
        , choices = initChoices.choices
        , ignoreList = []
        , selectedIndex = Nothing
        , fetcher = fetcher
        , isFetching = False
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
                    Debounce.push
                        state.debounceConfig
                        q
                        state.debounceState
            in
            ( Autocomplete
                { state
                    | query = q
                    , isFetching = True
                    , selectedIndex = Nothing
                    , debounceState = debounceState
                }
            , debounceCmd
            )

        DebounceMsg debouceMsg ->
            let
                doFetchCmd : String -> Cmd (Msg a)
                doFetchCmd s =
                    Task.perform DoFetch <| Task.succeed s

                ( debounceState, debounceCmd ) =
                    Debounce.update
                        state.debounceConfig
                        (Debounce.takeLast doFetchCmd)
                        debouceMsg
                        state.debounceState
            in
            ( Autocomplete { state | debounceState = debounceState }
            , debounceCmd
            )

        DoFetch s ->
            ( Autocomplete state
            , Task.perform OnFetch <| state.fetcher s
            )

        OnFetch c ->
            -- Racing condition between multiple fetches
            if c.query == state.query then
                ( Autocomplete
                    { state
                        | choices = List.filter (\i -> not <| List.member i state.ignoreList) c.choices
                        , isFetching = False
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
            ( Autocomplete { state | mouseDownIndex = Just index }
            , Cmd.none
            )

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



-- Accessors


viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , selectedIndex = s.selectedIndex
    , isFetching = s.isFetching
    }


reset : Choices a -> Autocomplete a -> Autocomplete a
reset c (Autocomplete s) =
    init c s.fetcher


setIgnoreList : List a -> Autocomplete a -> Autocomplete a
setIgnoreList ignoreList (Autocomplete s) =
    Autocomplete { s | ignoreList = ignoreList }


query : Autocomplete a -> String
query (Autocomplete s) =
    s.query


choices : Autocomplete a -> List a
choices (Autocomplete s) =
    s.choices


isFetching : Autocomplete a -> Bool
isFetching (Autocomplete s) =
    s.isFetching


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
