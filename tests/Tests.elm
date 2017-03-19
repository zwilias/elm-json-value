module Tests exposing (..)

import Test exposing (..)
import Expect
import Json
import Json.Decode
import Json.Encode


jsonOne : String
jsonOne =
    """
{"a": "b", "b": "c"}
"""


jsonTwo : String
jsonTwo =
    """
{"b": "c", "a": "b"}
"""


jsonThree : String
jsonThree =
    """"
{ "a": "d" }
"""


asJsonValue : String -> Json.Value
asJsonValue string =
    Json.Decode.decodeString Json.Decode.value string
        |> Result.withDefault Json.Encode.null


all : Test
all =
    describe "test"
        [ test "equals" <|
            \() ->
                Json.eq
                    (asJsonValue jsonOne)
                    (asJsonValue jsonTwo)
                    |> Expect.true "nope"
        , test "equals" <|
            \() ->
                Json.eq
                    (asJsonValue jsonOne)
                    (asJsonValue jsonThree)
                    |> Expect.false "nope"
        ]
