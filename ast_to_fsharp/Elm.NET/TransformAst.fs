module TransformAst

open ElmAst
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp
open System

let getNodeValue<'a> (node: 'a Node): 'a = 
    let (_, value) = node
    value

let offsetText (column: int) =
    String.replicate column " "

let moduleNameText (moduleName: ModuleName): string = 
    moduleName |> String.concat "." 

let moduleText (``module``: Module): string =
    let moduleText =
        match ``module`` with
        | NormalModule a -> a.moduleName |> getNodeValue |> moduleNameText
        | EffectModule a -> a.moduleName |> getNodeValue |> moduleNameText
        | PortModule a -> a.moduleName |> getNodeValue |> moduleNameText
    "module " + moduleText

let getExposing (file: File): Exposing =
    let ``module`` = getNodeValue file.moduleDefinition
    match ``module`` with
    | NormalModule a -> a.exposingList |> getNodeValue
    | EffectModule a -> a.exposingList |> getNodeValue
    | PortModule a -> a.exposingList |> getNodeValue

let rec patternText (pattern: Pattern): string =
    match pattern with
    | AllPattern -> ""
    | UnitPattern -> "()"
    | CharPattern a -> "'" + string a + "'"
    | StringPattern a -> "\"" + a + "\""
    | IntPattern a -> string a + "L"
    | HexPattern a -> string a + "L" //TODO: represent as hex literal
    | FloatPattern a -> string a
    | TuplePattern a -> 
        let tuples = a |> List.map (getNodeValue >> patternText) |> String.concat ", "
        "(" + tuples + ")"
    | RecordPattern a -> 
        let record = a |> List.map getNodeValue |> String.concat ", "
        "{" + record + "}"
    | UnConsPattern (a, b) -> ""
    | ListPattern a -> ""
    | VarPattern a -> a
    | NamedPattern (a, b) -> ""
    | AsPattern (a, b) -> ""
    | ParenthesizedPattern a -> ""

let declarationPrefix (isFirst: bool) (isPrivate: bool) = 
    let letText = 
        if isFirst then
            "let rec "
        else
            "and "
        
    let privateText = 
        if isPrivate then
            "private "
        else 
            ""
    letText + privateText


let ``functionText`` (column: int) (isFirst: bool) (isPrivate: bool) (``function``: Function): string =
    let implementation = ``function``.declaration |> getNodeValue 
    let functionName = implementation.name |> getNodeValue
    let arguments = implementation.arguments |> List.map (getNodeValue >> patternText >> Helper.flip (+) " ") |> String.concat ""

    declarationPrefix isFirst isPrivate + functionName + " " + arguments + "=" + "\n"


let declarationsText (exposing: Exposing) (declaration: Declaration List): string = 
    let functions = 
        declaration
        |> Helper.filterMap 
            (fun decl -> 
                match decl with
                | FunctionDeclaration ``function`` -> Some ``function``
                | _ -> None)
        |> List.indexed
        |> List.map (fun (index, func) -> functionText 0 (index = 0) true func)
        |> String.concat "\n\n"
    functions


    //let mutable isFirstLet = true
    //match declaration with
    //| FunctionDeclaration ``function`` -> 
    //    let isPrivate = 
    //        match exposing with
    //        | All _ -> false
    //        | Explicit exposingList -> 
    //            exposingList 
    //            |> List.map getNodeValue 
    //            |> List.exists (fun a -> ``function``.declaration |> getNodeValue |> (fun a -> a.name) |> getNodeValue |> FunctionExpose |> (=) a)

    //    let name = 
    //        if isFirstLet then
    //            "let "
    //        else
    //            "and "
        
    //    isFirstLet <- false

    //    if isPrivate then
    //        name + "private "
    //    else
    //        name

    //| _ -> 
    //    ""//raise (new NotImplementedException())

let file (file: File): string =
    let moduleDefinitionText = 
        file.moduleDefinition |> getNodeValue |> moduleText
    
    //let typeDeclarationsText = 
    //    file.declarations
    //    |> List.filterMap (getNodeValue >> )
    //    |> List.indexed
    //    |> List.map (fun (index, value) -> 
    //        let prefix = if index = 0 then "let " else "and "
    //        value |> getNodeValue |> declarationText (getExposing file)) 
    //    |> String.concat "\n\n"

    let declarationsText = 
        file.declarations 
        |> List.map getNodeValue
        |> declarationsText (getExposing file) 

    moduleDefinitionText + "\n" +
    "\n" +
    declarationsText + "\n"



let rec expr column expr = 
    match expr with
    | IfBlock ((_, ifExpr), (_, thenExpr), (_, elseExpr)) -> ifThenElse column ifExpr thenExpr elseExpr
    | _ -> ""

and ifThenElse column ifExpr thenExpr elseExpr =
    let offset = offsetText column
    "if " + (expr (column + 3) ifExpr) + " then\n" + offset +
    "    " + (expr (column + 4) thenExpr) + "\n" + offset +
    "else\n" + offset +
    "    " + (expr (column + 4) elseExpr) + "\n"

//let caseOf column caseExpr cases =


let transform (elmAst : File List) = 
    0