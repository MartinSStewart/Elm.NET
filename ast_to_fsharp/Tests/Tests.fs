module Tests

open System
open Xunit
open Import
open ElmAst
open System.Resources

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
    let result = ExampleAst.json |> import 
     
    Assert.Equal(expected, result)
