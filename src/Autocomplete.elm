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
    , selectedIndex
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
    , choices : a
    , choicesLen : Int
    , selectedIndex : Maybe Int
    , fetcher : String -> Task Never (Choices a)
    , isFetching : Bool
    , debounceConfig : Debounce.Config (Msg a)
    , debounceState : Debounce String
    }


type alias ViewState a =
    { query : String
    , choices : a
    , choicesLen : Int
    , selectedIndex : Maybe Int
    , isFetching : Bool
    }


init : Choices a -> (String -> Task Never (Choices a)) -> Autocomplete a
init initChoices fetcher =
    Autocomplete
        { query = ""
        , choices = initChoices.choices
        , choicesLen = initChoices.length
        , selectedIndex = Nothing
        , fetcher = fetcher
        , isFetching = False
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
            ( Autocomplete
                { state
                    | choices = c.choices
                    , choicesLen = c.length
                    , isFetching = False
                }
            , Cmd.none
            )

        OnKeyDown keyDown ->
            ( Autocomplete
                { state
                    | selectedIndex =
                        Internal.calculateIndex
                            state.choicesLen
                            state.selectedIndex
                            keyDown
                }
            , Cmd.none
            )

        OnMouseUp index ->
            -- TODO Probably need to check if it is out of range??? isFetching??
            ( Autocomplete { state | selectedIndex = Just index }
            , Cmd.none
            )



-- Accessors


viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , choicesLen = s.choicesLen
    , selectedIndex = s.selectedIndex
    , isFetching = s.isFetching
    }


query : Autocomplete a -> String
query (Autocomplete s) =
    s.query


choices : Autocomplete a -> a
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
