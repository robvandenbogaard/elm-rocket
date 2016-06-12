module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space
        37 ->
           ArrowLeft
        39 ->
            ArrowRight
        _ ->
            Unknown

toCode : Key -> Int
toCode key =
    case key of
        Space ->
            32
        ArrowLeft ->
            37
        ArrowRight ->
            39
        _ ->
            0
