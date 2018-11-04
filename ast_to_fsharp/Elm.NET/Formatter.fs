module Formatter

open Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp
open System

let offsetText (column: int) =
    String.replicate column " "

let tab = "    "

let moduleNameText (moduleName: ModuleName): string = 
    moduleName |> String.concat "." 
    
let documentationText (documentation: Documentation): string = 
    "/// <summary>\n" +
    "/// " + documentation + "\n" +
    "/// </summary>"

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

let letPrefix (isFirst: bool) (isExposed: bool) = 
    let letText = 
        if isFirst then
            "let rec "
        else
            "and "
        
    let privateText = 
        if isExposed then
            ""
        else 
            "private "
    letText + privateText

let typePrefix (isFirst: bool) (isExposed: bool) = 
    let typeText = 
        if isFirst then
            "type rec "
        else
            "and "
        
    let privateText = 
        if isExposed then
            ""
        else 
            "private "
    typeText + privateText

let ``functionText`` (column: int) (``function``: Function): string =
    let implementation = ``function``.declaration
    let functionName = implementation.name
    let arguments = 
        implementation.arguments 
        |> List.map (patternText >> Helper.flip (+) " ") 
        |> String.concat ""

    functionName + " " + arguments + "=\n"

let rec typeAnnotationText (typeAnnotation: TypeAnnotation): string =
    match typeAnnotation with
    | GenericType a -> a
    | Typed ((a, b), c) -> 
        let innerType = 
            if c.IsEmpty then
                ""
            else
                c |> List.map typeAnnotationText |> String.concat ", " |> (fun item -> "<" + item + ">")

        let outerType = 
            List.append a [ b ] |> String.concat "."
        outerType + innerType
    | Unit -> "()"
    | Tupled a -> 
        a 
        |> List.map typeAnnotationText 
        |> String.concat "\n, " 
        |> (fun b -> "( " + b + ")")
    | Record a -> 
        let recordFields = 
            a |> List.map recordFieldText |> String.concat ("\n" + tab + ",")

        tab + "{ " + recordFields +
        tab + "\n" + tab + "}"
    | GenericRecord (a, b) -> 
        let recordFields = 
            b |> List.map recordFieldText |> String.concat ("\n" + tab + tab)

        tab + "{ " + a + " |\n" +
        tab + tab + "| " + recordFields +
        tab + "}"
    | FunctionTypeAnnotation (a, b) -> 
        "(" + typeAnnotationText a + ") -> (" + typeAnnotationText b + ")"

and recordFieldText (fieldName, typeAnnotation) = 
    fieldName + " : " + typeAnnotationText typeAnnotation

let typeAliasText (isFirst: bool) (isExposed: bool) (typeAlias: TypeAlias): string =
    let generics = 
        typeAlias.generics |> List.map ((+) " ") |> String.concat ""

    let documentation = 
        match typeAlias.documentation with
        | Some a -> documentationText a + "\n"
        | None -> ""

    let body = 
        "    " + typeAnnotationText typeAlias.typeAnnotation

    documentation +
    typePrefix isFirst isExposed + typeAlias.name + generics + " = \n" +
    body

let typeDeclarationText (isFirst: bool) (typeDeclaration: TypeDeclaration): string = 
    match typeDeclaration with
    | AliasDeclaration (a, isExposed) -> typeAliasText isFirst isExposed a
    | CustomTypeDeclaration (a, isExposed) -> ""

let functionDeclarationText (isFirst: bool) (functionDeclaration: Declaration): string = 
    let letPrefix_ = letPrefix isFirst
    match functionDeclaration with
    | FunctionDeclaration (a, isExposed) -> letPrefix_ isExposed + functionText 0 a
    | PortDeclaration (a, isExposed) -> raise (new NotImplementedException())
    | InfixDeclaration (a, isExposed) -> raise (new NotImplementedException())
    | Destructuring (a, b, isExposed) -> raise (new NotImplementedException())


let fileText (file: File): string =
    let moduleDefinitionText = 
        "module " + moduleNameText file.moduleDefinition 
    
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
    typeDeclarationText + "\n" +
    "\n" +
    functionDeclarationText


let projectText (files: File List): string List = 
    List.map fileText files