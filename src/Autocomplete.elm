module Autocomplete exposing
    ( Autocomplete
    , Msg
    , init
    , isFetching
    , suggestions
    , update
    )

import Debounce exposing (Debounce)
import Task exposing (Task)
import Type as T exposing (Msg(..))


type alias Msg =
    T.Msg


type Autocomplete
    = Autocomplete State


type alias State =
    { query : String
    , suggestions : Result String (List String)
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
            ( Autocomplete { state | isFetching = True }
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



-- Accessors


suggestions : Autocomplete -> Result String (List String)
suggestions (Autocomplete state) =
    state.suggestions


isFetching : Autocomplete -> Bool
isFetching (Autocomplete state) =
    state.isFetching
