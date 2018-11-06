module Formatter

open Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp
open System
open Helper

let offsetText (column: int) =
    String.replicate column " "

let tab = "    "

let moduleNameText (moduleName: ModuleName): string = 
    moduleName |> String.concat "." 

let documentationText (documentation: Documentation): string = 
    let body = 
        documentation.Split("\n") 
        |> Seq.toList 
        |> List.map (quoteWith "/// " "\n") 
        |> String.concat ""
    "/// <summary>\n" +
    body + 
    "/// </summary>"

let rec patternText (column: int) (pattern: Pattern): string =
    match pattern with
    | AllPattern -> "_"
    | UnitPattern -> "()"
    | CharPattern a -> "'" + string a + "'"
    | StringPattern a -> "\"" + a + "\""
    | IntPattern a -> string a + "L"
    | HexPattern a -> string a + "L" //TODO: represent as hex literal
    | FloatPattern a -> string a
    | TuplePattern a -> 
        let tuples = a |> List.map (patternText column) |> String.concat ", "
        "(" + tuples + ")"
    | RecordPattern a -> 
        let record = a |> String.concat ", "
        "{" + record + "}"
    | UnConsPattern (a, b) -> 
        let left = patternText column a |> quoteWith "(" ")"
        let right = patternText column a |> quoteWith "(" ")"
        left + " :: " + right
    | ListPattern a -> 
        a 
        |> List.map (patternText (column + 2)) 
        |> String.concat ("\n" + (offsetText column) + ", ") 
        |> quoteWith "[ " (offsetText column + "]")
    | VarPattern a -> a
    | NamedPattern (a, b) -> ""
    | AsPattern (a, b) -> ""
    | ParenthesizedPattern a -> patternText column a |> quoteWith "(" ")"

let rec expressionText (column: int) (expression: Expression): string =
    match expression with
    | UnitExpr -> "()"
    | Application a -> raise (new NotImplementedException())
    | OperatorApplication (a, b, c, d) -> raise (new NotImplementedException())
    | FunctionOrValue (a, b) -> raise (new NotImplementedException())
    | IfBlock (ifGuard, thenGuard, elseGuard) -> 
        let ifText = expressionText (column + 3) ifGuard
        let thenText = expressionText (column + 4) thenGuard
        let elseText = expressionText (column + 4) elseGuard
        "if " + ifText + " then\n" +
        tab + thenText + "\n" +
        "else\n" +
        tab + elseText
    | PrefixOperator a -> a
    | Operator a -> a
    | Integer a -> string a
    | Hex a -> string a
    | Floatable a -> string a
    | Negation a -> expressionText (column + 2) a |> quoteWith "-(" ")"
    | Literal a -> a |> quote
    | CharLiteral a -> a |> string |> quoteWith "\'" "\'"
    | TupledExpression a -> 
        let tuples = a |> List.map (expressionText column) |> String.concat ", "
        "(" + tuples + ")"
    | ParenthesizedExpression a -> expressionText column a |> quoteWith "(" ")"
    | LetExpression a -> letBlockText (column + 4) a
    | CaseExpression a -> caseBlockText (column + 4) a
    | LambdaExpression a -> lambdaText (column + 4) a
    | RecordExpr a -> raise (new NotImplementedException())
    | ListExpr a -> raise (new NotImplementedException())
    | RecordAccess (a, b) -> raise (new NotImplementedException())
    | RecordAccessFunction a -> a
    | RecordUpdateExpression (a, b) -> raise (new NotImplementedException())
    | GLSLExpression a -> a

and letBlockText (column: int) (letBlock: LetBlock): string =
    raise (new NotImplementedException())

and caseBlockText (column: int) (letBlock: CaseBlock): string =
    raise (new NotImplementedException())

and lambdaText (column: int) (lambda: Lambda): string =
    raise (new NotImplementedException())

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
        |> List.map (patternText (column + 4) >> Helper.flip (+) " ") 
        |> String.concat ""
    let body = expressionText 4 implementation.expression

    functionName + " " + arguments + "=\n" + 
    body

let rec typeAnnotationText (typeAnnotation: TypeAnnotation): string =
    match typeAnnotation with
    | GenericType a -> a
    | Typed ((a, b), c) -> 
        let innerType = 
            if c.IsEmpty then
                ""
            else
                c 
                |> List.map typeAnnotationText 
                |> String.concat ", " 
                |> quoteWith "<" ">"

        let outerType = 
            List.append a [ b ] |> String.concat "."
        outerType + innerType
    | Unit -> "()"
    | Tupled a -> 
        a 
        |> List.map typeAnnotationText 
        |> String.concat "\n, " 
        |> quoteWith "( " ")"
    | Record a -> 
        let recordFields = 
            a |> List.map recordFieldText |> String.concat ("\n" + tab + ", ")

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
        typeAnnotationText typeAlias.typeAnnotation

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
    | InfixDeclaration (a, isExposed) -> ""
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