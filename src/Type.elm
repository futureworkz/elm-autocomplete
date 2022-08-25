module Type exposing (Msg(..))

import Debounce



-- This file is not exposed as a module
-- in order to hide Msg(..) from public usage


type Msg
    = OnInput String
    | DoFetch String
    | OnFetch (Result String (List String))
    | DebounceMsg Debounce.Msg
