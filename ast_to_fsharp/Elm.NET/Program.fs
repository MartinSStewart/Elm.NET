open System
open CommandLine
open System.IO

[<EntryPoint>]
let main argv =
    let command = "node elm.js " + Path.Combine("..", "..", "..", "..", "..", "elm_to_ast", "src", "Main.elm")
    let result = run command
    let searchText = "[{"
    let index = result.IndexOf(searchText)
    let endIndex = result.LastIndexOf("}]") + 2
    let a = 
        if index = -1 then
            None
        else
            let jsonText = result.Substring(index, endIndex - index)
            let parseResult = Import.import jsonText
            let code = parseResult |> Ast.fromProject |> Formatter.projectText
            Some code
    0
