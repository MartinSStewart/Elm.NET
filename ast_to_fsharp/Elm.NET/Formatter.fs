module Formatter

open Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp
open System

let offsetText (column: int) =
    String.replicate column " "

let moduleNameText (moduleName: ModuleName): string = 
    moduleName |> String.concat "." 

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
        let tuples = a |> List.map patternText |> String.concat ", "
        "(" + tuples + ")"
    | RecordPattern a -> 
        let record = a |> String.concat ", "
        "{" + record + "}"
    | UnConsPattern (a, b) -> ""
    | ListPattern a -> ""
    | VarPattern a -> a
    | NamedPattern (a, b) -> ""
    | AsPattern (a, b) -> ""
    | ParenthesizedPattern a -> ""

let letPrefix (isFirst: bool) (isPrivate: bool) = 
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

let typePrefix (isFirst: bool) (isPrivate: bool) = 
    let typeText = 
        if isFirst then
            "type rec "
        else
            "and "
        
    let privateText = 
        if isPrivate then
            "private "
        else 
            ""
    typeText + privateText

let ``functionText`` (column: int) (``function``: Function): string =
    let implementation = ``function``.declaration
    let functionName = implementation.name
    let arguments = implementation.arguments 
                    |> List.map (patternText >> Helper.flip (+) " ") 
                    |> String.concat ""

    functionName + " " + arguments + "=" + "\n"

let recordDeclarationText (isFirst: bool) (isPrivate: bool) (typeAlias: TypeAlias): string =
    typePrefix isFirst isPrivate

let typeDeclarationText (isFirst: bool) (typeDeclaration: TypeDeclaration): string = 
    let typePrefix_ = typePrefix isFirst
    match typeDeclaration with
    | AliasDeclaration (a, isExposed) -> typePrefix_ isExposed
    | CustomTypeDeclaration (a, isExposed) -> typePrefix_ isExposed

let functionDeclarationText (isFirst: bool) (functionDeclaration: Declaration): string = 
    let letPrefix_ = letPrefix isFirst
    match functionDeclaration with
    | FunctionDeclaration (a, isExposed) -> letPrefix_ isExposed + functionText 0 a
    | PortDeclaration (a, isExposed) -> raise (new NotImplementedException())
    | InfixDeclaration (a, isExposed) -> raise (new NotImplementedException())
    | Destructuring (a, b, isExposed) -> raise (new NotImplementedException())

//let declarationsText (declaration: Declaration List): string = 
//    let types = 
//        declaration
//        |> List.indexed
//        |> Helper.filterMap 
//            (fun (index, decl) -> 
//                match decl with
//                | AliasDeclaration alias -> alias |> recordDeclarationText (index = 0) true |> Some
//                | CustomTypeDeclaration customType -> Some ""
//                | _ -> None)
//        |> String.concat "\n\n"

//    let functions = 
//        declaration
//        |> Helper.filterMap 
//            (fun decl -> 
//                match decl with
//                | FunctionDeclaration ``function`` -> Some ``function``
//                | _ -> None)
//        |> List.indexed
//        |> List.map (fun (index, func) -> functionText 0 (index = 0) true func)
//        |> String.concat "\n\n"
//    types + "\n" +
//    "\n" +
//    functions


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

let fileText (file: File): string =
    let moduleDefinitionText = 
        moduleNameText file.moduleDefinition 
    
    let typeDeclarationText = 
        file.typeDeclarations 
        |> List.indexed 
        |> List.map (fun (index, a) -> typeDeclarationText (index = 0) a) 
        |> String.concat "\n\n"

    let functionDeclarationText = 
        file.declarations
        |> List.indexed
        |> List.map (fun (index, a) -> functionDeclarationText (index = 0) a) 
        |> String.concat "\n\n"

    moduleDefinitionText + "\n" +
    "\n" +
    typeDeclarationText + "\n"



//let rec expr column expr = 
//    match expr with
//    | IfBlock ((_, ifExpr), (_, thenExpr), (_, elseExpr)) -> ifThenElse column ifExpr thenExpr elseExpr
//    | _ -> ""

//and ifThenElse column ifExpr thenExpr elseExpr =
//    let offset = offsetText column
//    "if " + (expr (column + 3) ifExpr) + " then\n" + offset +
//    "    " + (expr (column + 4) thenExpr) + "\n" + offset +
//    "else\n" + offset +
//    "    " + (expr (column + 4) elseExpr) + "\n"

//let caseOf column caseExpr cases =


let transform (elmAst : File List) = 
    0