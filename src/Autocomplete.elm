module Autocomplete exposing
    ( Autocomplete
    , Choices
    , Msg
    , ViewState
    , ViewStatus(..)
    , choices
    , init
    , isFetching
    , isSelected
    , query
    , reset
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
    , choices : List a
    , ignoreList : List a
    , fetcher : Choices a -> Task String (Choices a)
    , isFetching : Bool
    , error : Maybe String
    , selectedIndex : Maybe Int
    , mouseDownIndex : Maybe Int
    , debounceConfig : Debounce.Config (Msg a)
    , debounceState : Debounce String
    }


type alias ViewState a =
    { query : String
    , choices : List a
    , selectedIndex : Maybe Int
    , status : ViewStatus
    }


type ViewStatus
    = Error String
    | Fetching
    | Ready


init : Choices a -> (Choices a -> Task String (Choices a)) -> Autocomplete a
init initChoices fetcher =
    Autocomplete
        { query = initChoices.query
        , choices = initChoices.choices
        , ignoreList = initChoices.ignoreList
        , fetcher = fetcher
        , isFetching = False
        , error = Nothing
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
                    , error = Nothing
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
                    ( Autocomplete { state | error = Just s }, Cmd.none )

                Ok c ->
                    if c.query == state.query then
                        ( Autocomplete
                            { state
                                | choices =
                                    List.filter
                                        (\i -> not <| List.member i c.ignoreList)
                                        c.choices
                                , ignoreList = c.ignoreList
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



-- Accessors


viewState : Autocomplete a -> ViewState a
viewState (Autocomplete s) =
    { query = s.query
    , choices = s.choices
    , selectedIndex = s.selectedIndex
    , status =
        case ( s.error, s.isFetching ) of
            ( Just e, _ ) ->
                Error e

            ( _, True ) ->
                Fetching

            _ ->
                Ready
    }


reset : Choices a -> Autocomplete a -> Autocomplete a
reset c (Autocomplete s) =
    init c s.fetcher


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
