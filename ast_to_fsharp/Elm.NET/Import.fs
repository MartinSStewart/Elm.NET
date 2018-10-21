module Import

open Newtonsoft.Json
open ElmAst
open Newtonsoft.Json.Linq
open System

let decodeList<'a, 'b> (decoder : 'b -> 'a) (json : JArray): List<'a> =
    json.Values() |> List.ofSeq |> List.map decoder

let decodeRange (json : JArray): Range = 
    let values = decodeList int json
    { start = { row = values.[0]; column = values.[1] }
    ; ``end`` = { row = values.[2]; column = values.[3] }
    }

let decodeNode<'a, 'b> (decoder : 'b -> 'a) (json : JObject): Node<'a> =
    ( json.Value<JArray>("range") |> decodeRange
    , json.Value<'b>("value") |> decoder
    )

let decodeModuleName (json : JArray): ModuleName =
    decodeList string json

let decodeExposedType (json : JObject): ExposedType =
    { name = json.Value<string>("name"); ``open`` = raise (new NotImplementedException()) }

let decodeTopLevelExpose (json : JObject): TopLevelExpose =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "infix" -> 
        json.Value<string>("infix") 
        |> InfixExpose
    | "function" -> 
        json.Value<JObject>("function") 
        |> (fun a -> a.Value<string>("name"))
        |> FunctionExpose
    | "typeOrAlias" -> 
        json.Value<string>("typeOrAlias") 
        |> TypeOrAliasExpose
    | "typeexpose" -> 
        json.Value<JObject>("typeexpose") 
        |> decodeExposedType
        |> TypeExpose
    | _ -> raise (new NotImplementedException())

let decodeExposing (json : JObject): Exposing =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "all" -> 
        json.Value<JArray>("all") 
        |> decodeRange 
        |> All
    | "explicit" -> 
        json.Value<JArray>("explicit") 
        |> decodeList (decodeNode decodeTopLevelExpose)
        |> Explicit
    | _ -> raise (new NotImplementedException())

let decodeDefaultModuleData (json : JObject): DefaultModuleData =
    { moduleName = decodeNode decodeModuleName (json.Value<JObject>("moduleName"))
    ; exposingList = decodeNode decodeExposing (json.Value<JObject>("exposingList"))
    }

let decodeEffectModuleData (json : JObject): EffectModuleData =
    { moduleName = decodeNode decodeModuleName (json.Value<JObject>("moduleName"))
    ; exposingList = decodeNode decodeExposing (json.Value<JObject>("exposingList"))
    ; command = raise (new NotImplementedException())
    ; subscription = raise (new NotImplementedException())
    }

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
    decodeNode decodeModule json

let decodeFile (json : JObject): File =
    { moduleDefinition = decodeModuleDefinition <| json.Value<JObject>("moduleDefinition")
    ; imports = []
    ; declarations = []
    ; comments = []
    }

// Loads the json containing the Elm AST and parses it.
let import text:File =
    let json = JObject.Parse(text)

    decodeFile json