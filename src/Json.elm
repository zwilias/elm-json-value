module Json exposing (Value, eq)

import Json.Encode
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)


type alias Value =
    Json.Encode.Value


type JsonValue
    = NullVal
    | StringVal String
    | FloatVal Float
    | IntVal Int
    | ListVal (List JsonValue)
    | DictVal (Dict String JsonValue)


valueDecoder : Decoder JsonValue
valueDecoder =
    Decode.oneOf
        [ Decode.map StringVal Decode.string
        , Decode.map FloatVal Decode.float
        , Decode.map IntVal Decode.int
        , Decode.map ListVal <| Decode.list (Decode.lazy (\() -> valueDecoder))
        , Decode.map DictVal <| Decode.dict (Decode.lazy (\() -> valueDecoder))
        , Decode.null NullVal
        ]


eqHelper : JsonValue -> JsonValue -> Bool
eqHelper left right =
    case ( left, right ) of
        ( NullVal, NullVal ) ->
            True

        ( StringVal leftVal, StringVal rightVal ) ->
            leftVal == rightVal

        ( FloatVal leftVal, FloatVal rightVal ) ->
            leftVal == rightVal

        ( IntVal leftVal, IntVal rightVal ) ->
            leftVal == rightVal

        ( ListVal leftVal, ListVal rightVal ) ->
            List.length leftVal
                == List.length rightVal
                && (List.map2 eqHelper leftVal rightVal
                        |> List.all ((==) True)
                   )

        ( DictVal leftVal, DictVal rightVal ) ->
            let
                leftList =
                    Dict.toList leftVal

                rightList =
                    Dict.toList rightVal
            in
                List.length leftList
                    == List.length rightList
                    && (List.map2
                            (\( lKey, lVal ) ( rKey, rVal ) ->
                                lKey == rKey && eqHelper lVal rVal
                            )
                            leftList
                            rightList
                            |> List.all ((==) True)
                       )

        ( _, _ ) ->
            False


eq : Value -> Value -> Bool
eq left right =
    Result.map2
        eqHelper
        (Decode.decodeValue valueDecoder left)
        (Decode.decodeValue valueDecoder right)
        |> Result.withDefault False
