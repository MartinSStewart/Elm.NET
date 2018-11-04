module ElmJson

open Newtonsoft.Json.Linq
open Helper
open System

type Version = String

type Type = Application | Package

type PackageName = 
    { owner: string
    ; name: string
    }

type Package = 
    { packageName: PackageName
    ; version: Version 
    }

type Dependency =
    { direct: Package List
    ; indirect: Package List
    }

type ElmJson = 
    { dependencies: Dependency
    ; sourceDirectories: string List
    ; testDependencies: Dependency
    ; elmVersion: Version
    ; ``type``: Type 
    }

let decodeType (json: JToken): Type =
    match decodeString json with
    | "application" -> Application
    | "package" -> Package
    | _ -> raise (new NotImplementedException())

let decodeVersion (json: JToken): Version = 
    json |> decodeString

let decodePackageName (text: string): PackageName =
    let split = text.Split('/')
    { owner = split.[0]
    ; name = split.[1]
    }

let decodePackage (json: JObject): Package List =
    json.Values<JProperty>() 
    |> List.ofSeq 
    |> List.map 
        (fun a -> 
            { packageName = decodePackageName a.Name
            ; version = decodeString a.Value 
            } 
        ) 

let decodeDependency (json: JObject): Dependency =
    { direct = json.Value<JObject>("direct") |> decodePackage
    ; indirect = json.Value<JObject>("indirect") |> decodePackage
    }

let decodeElmJson (json: JObject): ElmJson = 
    { dependencies = json.Value<JObject>("dependencies") |> decodeDependency
    ; sourceDirectories = json.Value<JArray>("source-directories") |> decodeList decodeString
    ; testDependencies = json.Value<JObject>("test-dependencies") |> decodeDependency
    ; elmVersion = json.Value<JToken>("elm-version") |> decodeVersion
    ; ``type`` = json.Value<JToken>("type") |> decodeType
    }

let import (text: string): ElmJson =
    JObject.Parse(text) |> decodeElmJson