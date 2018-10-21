﻿module Tests

open System
open Xunit
open Import
open ElmAst

let exampleJsonText = 
    "{ \"moduleDefinition\": { \"range\": [ 1, 1, 1, 36 ], \"value\": { \"type\": \"normal\", \"normal\": { \"moduleName\": { \"range\": [ 1, 8, 1, 12 ], \"value\": [ \"Test\" ] }, \"exposingList\": { \"range\": [ 1, 13, 1, 36 ], \"value\": { \"type\": \"explicit\", \"explicit\": [ { \"range\": [ 1, 23, 1, 35 ], \"value\": { \"type\": \"function\", \"function\": { \"name\": \"initialModel\" } } } ] } } } } }, \"imports\": [], \"declarations\": [ { \"range\": [ 3, 1, 4, 7 ], \"value\": { \"type\": \"typeAlias\", \"typeAlias\": { \"documentation\": null, \"name\": { \"range\": [ 3, 12, 3, 17 ], \"value\": \"Model\" }, \"generics\": [], \"typeAnnotation\": { \"range\": [ 4, 5, 4, 7 ], \"value\": { \"type\": \"record\", \"record\": { \"value\": [] } } } } } }, { \"range\": [ 7, 1, 9, 7 ], \"value\": { \"type\": \"function\", \"function\": { \"documentation\": null, \"signature\": { \"range\": [ 7, 1, 7, 21 ], \"value\": { \"name\": { \"range\": [ 7, 1, 7, 13 ], \"value\": \"initialModel\" }, \"typeAnnotation\": { \"range\": [ 7, 16, 7, 21 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 7, 16, 7, 21 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } } } }, \"declaration\": { \"range\": [ 8, 1, 9, 7 ], \"value\": { \"name\": { \"range\": [ 8, 1, 8, 13 ], \"value\": \"initialModel\" }, \"arguments\": [], \"expression\": { \"range\": [ 9, 5, 9, 7 ], \"value\": { \"type\": \"record\", \"record\": [] } } } } } } }, { \"range\": [ 12, 1, 13, 11 ], \"value\": { \"type\": \"typedecl\", \"typedecl\": { \"documentation\": null, \"name\": { \"range\": [ 12, 6, 12, 9 ], \"value\": \"Msg\" }, \"generics\": [], \"constructors\": [ { \"range\": [ 13, 7, 13, 11 ], \"value\": { \"name\": { \"range\": [ 13, 7, 13, 11 ], \"value\": \"NoOp\" }, \"arguments\": [] } } ] } } }, { \"range\": [ 16, 1, 18, 10 ], \"value\": { \"type\": \"function\", \"function\": { \"documentation\": null, \"signature\": { \"range\": [ 16, 1, 16, 31 ], \"value\": { \"name\": { \"range\": [ 16, 1, 16, 7 ], \"value\": \"update\" }, \"typeAnnotation\": { \"range\": [ 16, 10, 16, 31 ], \"value\": { \"type\": \"function\", \"function\": { \"left\": { \"range\": [ 16, 10, 16, 13 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 10, 16, 13 ], \"value\": { \"moduleName\": [], \"name\": \"Msg\" } }, \"args\": [] } } }, \"right\": { \"range\": [ 16, 17, 16, 31 ], \"value\": { \"type\": \"function\", \"function\": { \"left\": { \"range\": [ 16, 17, 16, 22 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 17, 16, 22 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } }, \"right\": { \"range\": [ 16, 26, 16, 31 ], \"value\": { \"type\": \"typed\", \"typed\": { \"moduleNameAndName\": { \"range\": [ 16, 26, 16, 31 ], \"value\": { \"moduleName\": [], \"name\": \"Model\" } }, \"args\": [] } } } } } } } } } } }, \"declaration\": { \"range\": [ 17, 1, 18, 10 ], \"value\": { \"name\": { \"range\": [ 17, 1, 17, 7 ], \"value\": \"update\" }, \"arguments\": [ { \"range\": [ 17, 8, 17, 11 ], \"value\": { \"type\": \"var\", \"var\": { \"value\": \"msg\" } } }, { \"range\": [ 17, 12, 17, 17 ], \"value\": { \"type\": \"var\", \"var\": { \"value\": \"model\" } } } ], \"expression\": { \"range\": [ 18, 5, 18, 10 ], \"value\": { \"type\": \"functionOrValue\", \"functionOrValue\": { \"moduleName\": [], \"name\": \"model\" } } } } } } } } ], \"comments\": [] }"

    //[ { \"range\": [ 1, 23, 1, 35 ], \"value\": { \"type\": \"function\", \"function\": { \"name\": \"initialModel\" } } } ]

[<Fact>]
let ``Decode simple Elm AST json`` () =
    let expected = 
        { moduleDefinition =
            ( { start = {row = 1; column = 1}; ``end`` = {row = 1; column = 36} }
            , NormalModule {moduleName = ({start = {row = 1; column = 8}; ``end`` = {row = 1; column = 12}}, ["Test"])
        ; exposingList =
            ( { start = {row = 1; column = 13}; ``end`` = {row = 1; column = 36} }
            , Explicit [({start = {row = 1; column = 23}; ``end`` = {row = 1; column = 35}}, FunctionExpose "initialModel")])}
            )
        ; imports = []
        ; declarations = []
        ; comments = [] 
        }
    let result = exampleJsonText |> import 
     
    Assert.Equal(expected, result)