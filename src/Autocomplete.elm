module Autocomplete exposing
    ( Autocomplete
    , KeyDown
    , Msg
    , init
    , isFetching
    , query
    , selectedIndex
    , suggestions
    , update
    )

import Debounce exposing (Debounce)
import Internal exposing (KeyDown(..), Msg(..))
import Task exposing (Task)


type alias Msg e a =
    Internal.Msg e a


type alias KeyDown =
    Internal.KeyDown


type Autocomplete e a
    = Autocomplete (State e a)


type alias State e a =
    { query : String
    , suggestions : Result e (List a)
    , selectedIndex : Maybe Int
    , fetcher : String -> Task e (List a)
    , isFetching : Bool
    , debounceConfig : Debounce.Config (Msg e a)
    , debounceState : Debounce String
    }


init : (String -> Task e (List a)) -> Autocomplete e a
init fetcher =
    Autocomplete
        { query = ""
        , suggestions = Ok []
        , selectedIndex = Nothing
        , fetcher = fetcher
        , isFetching = False
        , debounceConfig =
            { strategy = Debounce.later 200
            , transform = DebounceMsg
            }
        , debounceState = Debounce.init
        }


update : Msg e a -> Autocomplete e a -> ( Autocomplete e a, Cmd (Msg e a) )
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
                    , selectedIndex = Nothing
                    , debounceState = debounceState
                }
            , debounceCmd
            )

        DebounceMsg debouceMsg ->
            let
                doFetchCmd : String -> Cmd (Msg e a)
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
            ( Autocomplete { state | isFetching = True, selectedIndex = Nothing }
            , Task.attempt OnFetch <| state.fetcher s
            )

        OnFetch s ->
            ( Autocomplete
                { state
                    | suggestions = s
                    , isFetching = False
                }
            , Cmd.none
            )

        OnKeyDown keyDown ->
            ( Autocomplete
                { state
                    | selectedIndex =
                        Internal.calculateIndex
                            (Result.withDefault [] state.suggestions)
                            state.selectedIndex
                            keyDown
                }
            , Cmd.none
            )



-- Accessors


query : Autocomplete e a -> String
query (Autocomplete state) =
    state.query


suggestions : Autocomplete e a -> Result e (List a)
suggestions (Autocomplete state) =
    state.suggestions


isFetching : Autocomplete e a -> Bool
isFetching (Autocomplete state) =
    state.isFetching


selectedIndex : Autocomplete e a -> Maybe Int
selectedIndex (Autocomplete state) =
    state.selectedIndex
