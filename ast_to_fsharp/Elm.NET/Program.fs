open System
open CommandLine
open System.IO

[<EntryPoint>]
let main argv =
    let rootFolder = Path.Combine("..", "..", "..", "..", "..", "elm_to_ast")
    let elmJson = File.ReadAllText(Path.Combine(rootFolder, "elm.json")) |> ElmJson.import

    let dependencies = 
        elmJson.dependencies.direct 
        |> List.append elmJson.dependencies.direct 
        |> Set.ofList

    let cacheDirectory = 
        Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), 
            "elm",
            "0.19.0",
            "package")

    let packageFiles = 
        dependencies 
        |> Set.map 
            (fun a -> 
                let path = Path.Combine(cacheDirectory, a.packageName.owner, a.packageName.name, a.version, "src")
                Directory.EnumerateFiles(path, "*.elm", SearchOption.AllDirectories) 
                |> Seq.toList 
            )
        |> List.concat

    let userFiles = 
        elmJson.sourceDirectories 
        |> List.map 
            (fun a -> 
                let path = Path.Combine(rootFolder, a)
                Directory.EnumerateFiles(path, "*.elm", SearchOption.AllDirectories) 
                |> Seq.toList 
            )
        |> List.concat

    //let allFiles = userFiles |> List.append packageFiles |> List.map Helper.quote |> String.concat " "
    let allFiles = 
        (Helper.quote @"C:\Users\Martin\AppData\Roaming\elm\0.19.0\package\stil4m\elm-syntax\7.0.2\src\Elm\Processing.elm")
    let command = "node elm.js " + allFiles
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
