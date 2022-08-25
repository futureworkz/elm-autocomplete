module Autocomplete exposing
    ( Autocomplete
    , Msg
    , init
    , isFetching
    , selectedIndex
    , suggestions
    , update
    )

import Debounce exposing (Debounce)
import Internal exposing (Msg(..))
import Task exposing (Task)


type alias Msg =
    Internal.Msg


type Autocomplete
    = Autocomplete State


type alias State =
    { query : String
    , suggestions : Result String (List String)
    , selectedIndex : Maybe Int
    , fetcher : String -> Task String (List String)
    , isFetching : Bool
    , debounceConfig : Debounce.Config Msg
    , debounceState : Debounce String
    }


init : (String -> Task String (List String)) -> Autocomplete
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


update : Msg -> Autocomplete -> ( Autocomplete, Cmd Msg )
update msg (Autocomplete state) =
    case msg of
        OnInput query ->
            let
                ( debounceState, debounceCmd ) =
                    Debounce.push
                        state.debounceConfig
                        query
                        state.debounceState
            in
            ( Autocomplete
                { state
                    | query = query
                    , selectedIndex = Nothing
                    , debounceState = debounceState
                }
            , debounceCmd
            )

        DebounceMsg debouceMsg ->
            let
                doFetchCmd : String -> Cmd Msg
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


suggestions : Autocomplete -> Result String (List String)
suggestions (Autocomplete state) =
    state.suggestions


isFetching : Autocomplete -> Bool
isFetching (Autocomplete state) =
    state.isFetching


selectedIndex : Autocomplete -> Maybe Int
selectedIndex (Autocomplete state) =
    state.selectedIndex
