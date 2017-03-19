module Json exposing (Value, eq)

import Json.Encode
import Json.Decode as Decode exposing (Decoder)
import Dict exposing (Dict)


type alias Value =
    Json.Encode.Value


type JsonValue
    = NVal
    | SVal String
    | FVal Float
    | IVal Int
    | LVal (List JsonValue)
    | DVal (Dict String JsonValue)


valueDecoder : Decoder JsonValue
valueDecoder =
    Decode.oneOf
        [ Decode.map SVal Decode.string
        , Decode.map FVal Decode.float
        , Decode.map IVal Decode.int
        , Decode.map LVal <| Decode.list (Decode.lazy (\() -> valueDecoder))
        , Decode.map DVal <| Decode.dict (Decode.lazy (\() -> valueDecoder))
        , Decode.null NVal
        ]


eqHelper : JsonValue -> JsonValue -> Bool
eqHelper left right =
    case ( left, right ) of
        ( NVal, NVal ) ->
            True

        ( SVal leftVal, SVal rightVal ) ->
            leftVal == rightVal

        ( FVal leftVal, FVal rightVal ) ->
            leftVal == rightVal

        ( IVal leftVal, IVal rightVal ) ->
            leftVal == rightVal

        ( LVal leftVal, LVal rightVal ) ->
            List.length leftVal
                == List.length rightVal
                && (List.map2 eqHelper leftVal rightVal
                        |> List.all ((==) True)
                   )

        ( DVal leftVal, DVal rightVal ) ->
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
