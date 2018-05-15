module Bot2 exposing (..)

import Json.Decode as Decode
import Http


type Action req
    = Ask String String
    | Request (Cmd (Msg req))


type alias Tree req =
    { id : String
    , actions : List (Action req)
    , formatter : req -> String
    }


type Msg req
    = WaitTimeout
    | RequestResult req


tree : String -> (req -> String) -> Tree req
tree id formatter =
    Tree id [] formatter


ask : String -> String -> Tree req -> Tree req
ask question key tree =
    { tree | actions = List.append tree.actions [ Ask question key ] }


request : String -> (Result Http.Error a -> req) -> Decode.Decoder a -> Tree req -> Tree req
request url toMsg decoder tree =
    let
        req =
            Http.get url decoder

        cmd =
            Http.send RequestResult req
    in
        { tree | actions = List.append tree.actions [ Request cmd ] }



-- { tree | actions = List.append tree.actions [ Request toMsg ] }
