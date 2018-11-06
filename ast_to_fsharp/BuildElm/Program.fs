open System
open System.IO
open CommandLine

[<EntryPoint>]
let main argv =

    let path = Path.Combine("..", "..", "..", "..", "..", "elm_to_ast")
    let elmJs = Path.Combine(path, "elm.js")
    let destinationElmJs = Path.Combine("..", "..", "..", "..", "Elm.NET", "bin", "Debug", "netcoreapp2.1", "elm.js")
    do File.Delete(destinationElmJs)
    let command = 
        "cd " + path + "\n" +
        "elm make " + Path.Combine("src", "Main.elm") + " --output elm.js"
    let _ = run command
    let fileText = 
        File.ReadAllText(elmJs) +
        "\n" +
        "\n" +
        "var fs = require('fs');\n" +
        "var input = fs.readFileSync('input.txt', 'utf8');" + 
        "var files = input.split(\"\\n\").map(function (val, index, array) {\n" +
        "    return fs.readFileSync(val, 'utf8');\n" +
        "});\n" +
        "var app = this.Elm.Main.init({flags: files});\n" +
        "app.ports.response.subscribe( ( message ) => {\n" +
        "    console.log(JSON.stringify(message));\n" +
        "    process.exit();" +
        "});\n"
    do File.WriteAllText(elmJs, fileText)
    do File.Move(elmJs, destinationElmJs)

    0