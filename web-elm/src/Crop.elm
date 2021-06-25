module Crop exposing (..)

import Json.Encode


type alias Crop =
    { left : Int
    , top : Int
    , right : Int
    , bottom : Int
    }


encodeCrop : Crop -> Json.Encode.Value
encodeCrop { left, top, right, bottom } =
    Json.Encode.object
        [ ( "left", Json.Encode.int left )
        , ( "top", Json.Encode.int top )
        , ( "right", Json.Encode.int right )
        , ( "bottom", Json.Encode.int bottom )
        ]
