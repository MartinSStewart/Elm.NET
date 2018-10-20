module Import

open Newtonsoft.Json
open ElmAst
open Newtonsoft.Json.Linq
open System

let exampleJson = 
    "{ \"moduleDefinition\": { \"range\": [ 1, 1, 1, 36 ], \"value\": { \"type\": \"normal\", \"normal\": { \"moduleName\": { \"range\": [ 1, 8, 1, 12 ], \"value\": [ \"Test\" ] }, \"exposingList\": { \"range\": [ 1, 13, 1, 36 ], \"value\": { \"type\": \"explicit\", \"explicit\": [ { \"range\": [ 1, 23, 1, 35 ], \"value\": { \"type\": \"function\", \"function\": { \"name\": \"initialModel\" } } } ] } } } } }, \"imports\": [], \"declarations\": [ { \"range\": [ 3, 1, 4, 7 ], \"value\": { \"type\": \"typeAlias\", \"typeAlias\": { \"documentation\": null, \"name\": { \"range\": [ 3, 12, 3, 17 ], \"value\": \"Model\" }, \"generics\": [], \"typeAnnotation\": { \"range\": [ 4, 5, 4, 7 ], \"value\": { \"type\": \"record\", \"record\": { \"value\": [] } } } } } }, { \"range\": [ 7, 1, 9, 7 ], \"value\": { \"type\": \"function\", \"function\": { \"documentation\": null, \"signature\": { \"range\": [ 7, 1, 7, 21 ], \"value\": { \"name\": { \"range\": [ 7, 1, 7, 13 ], \"value\": \"initialModel\" }, \"typeAnnotation\": { \"range\": [ 7, 16, 7, 21 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 7, 16, 7, 21 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } } } }, \"declaration\": { \"range\": [ 8, 1, 9, 7 ], \"value\": { \"name\": { \"range\": [ 8, 1, 8, 13 ], \"value\": \"initialModel\" }, \"arguments\": [], \"expression\": { \"range\": [ 9, 5, 9, 7 ], \"value\": { \"type\": \"record\", \"record\": [] } } } } } } }, { \"range\": [ 12, 1, 13, 11 ], \"value\": { \"type\": \"typedecl\", \"typedecl\": { \"documentation\": null, \"name\": { \"range\": [ 12, 6, 12, 9 ], \"value\": \"Msg\" }, \"generics\": [], \"constructors\": [ { \"range\": [ 13, 7, 13, 11 ], \"value\": { \"name\": { \"range\": [ 13, 7, 13, 11 ], \"value\": \"NoOp\" }, \"arguments\": [] } } ] } } }, { \"range\": [ 16, 1, 18, 10 ], \"value\": { \"type\": \"function\", \"function\": { \"documentation\": null, \"signature\": { \"range\": [ 16, 1, 16, 31 ], \"value\": { \"name\": { \"range\": [ 16, 1, 16, 7 ], \"value\": \"update\" }, \"typeAnnotation\": { \"range\": [ 16, 10, 16, 31 ], \"value\": { \"type\": \"function\", \"function\": { \"left\": { \"range\": [ 16, 10, 16, 13 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 10, 16, 13 ], \"value\": { \"moduleName\": [], \"name\": \"Msg\" } }, \"args\": [] } } }, \"right\": { \"range\": [ 16, 17, 16, 31 ], \"value\": { \"type\": \"function\", \"function\": { \"left\": { \"range\": [ 16, 17, 16, 22 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 17, 16, 22 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } }, \"right\": { \"range\": [ 16, 26, 16, 31 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 26, 16, 31 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } } } } } } } } } }, \"declaration\": { \"range\": [ 17, 1, 18, 10 ], \"value\": { \"name\": { \"range\": [ 17, 1, 17, 7 ], \"value\": \"update\" }, \"arguments\": [ { \"range\": [ 17, 8, 17, 11 ], \"value\": { \"type\": \"var\", \"var\": { \"value\": \"msg\" } } }, { \"range\": [ 17, 12, 17, 17 ], \"value\": { \"type\": \"var\", \"var\": { \"value\": \"model\" } } } ], \"expression\": { \"range\": [ 18, 5, 18, 10 ], \"value\": { \"type\": \"functionOrValue\", \"functionOrValue\": { \"moduleName\": [], \"name\": \"model\" } } } } } } } } ], \"comments\": [] }"

let decodeRange (json : JArray): Range = 
    { start = { row = (int)json.[0]; column = (int)json.[1] }
    ; ``end`` = { row = (int)json.[2]; column = (int)json.[3] }
    }

let decodeNode<'a, 'b> (json : JObject, decoder : 'b -> 'a): Node<'a> =
    (decodeRange <| json.Value<JArray>("range")
    , decoder <| json.Value<'b>("value")
    )

let decodeDefaultModuleData (json : JObject): DefaultModuleData =
    raise (new NotImplementedException())

let decodeEffectModuleData (json : JObject): EffectModuleData =
    raise (new NotImplementedException())

let decodeModule (json : JObject): Module =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "normal" ->
        json.Value<JObject>("normal") |> decodeDefaultModuleData |> NormalModule
    | "port" ->
        json.Value<JObject>("port") |> decodeDefaultModuleData |> PortModule
    | "effect" -> 
        json.Value<JObject>("effect") |> decodeEffectModuleData |> EffectModule
    | _ -> raise (new NotImplementedException())


let decodeModuleDefinition (json : JObject): Node<Module> =
    decodeNode (json, decodeModule)

let decodeFile (json : JObject): File =
    { moduleDefinition = decodeModuleDefinition <| json.Value<JObject>("moduleDefinition")
    ; imports = []
    ; declarations = []
    ; comments = []
    }

// Loads the json containing the Elm AST and parses it.
let import:File =
    let json = JObject.Parse(exampleJson)

    decodeFile json