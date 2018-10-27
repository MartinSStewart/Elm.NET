module Ast

open System

type ModuleName =
    List<string>

type QualifiedNameRef =
    { moduleName : List<string>
    ; name : string
    }

type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern of char
    | StringPattern of string
    | IntPattern of int64
    | HexPattern of int64
    | FloatPattern of double
    | TuplePattern of List<Pattern>
    | RecordPattern of List<string>
    | UnConsPattern of Pattern * Pattern
    | ListPattern of List<Pattern>
    | VarPattern of string
    | NamedPattern of QualifiedNameRef * List<Pattern>
    | AsPattern of Pattern * string
    | ParenthesizedPattern of Pattern

type Documentation =
    string

type RecordField =
    ( string * TypeAnnotation )

and RecordDefinition =
        List<RecordField>

and TypeAnnotation
    = GenericType of string
    | Typed of (ModuleName * string) * List<TypeAnnotation>
    | Unit
    | Tupled of List<TypeAnnotation>
    | Record of RecordDefinition
    | GenericRecord of string * RecordDefinition
    | FunctionTypeAnnotation of TypeAnnotation * TypeAnnotation

type ValueConstructor =
    { name : string
    ; arguments : List<TypeAnnotation>
    }

type Type =
    { documentation : Option<Documentation>
    ; name : string
    ; generics : List<string>
    ; constructors : List<ValueConstructor>
    }

type TypeAlias =
    { documentation : Option<Documentation>
    ; name : string
    ; generics : List<string>
    ; typeAnnotation : TypeAnnotation
    }

type InfixDirection
    = Left
    | Right
    | Non

type Infix =
    { direction : InfixDirection
    ; precedence : int
    ; operator : string
    ; ``function`` : string
    }

type Signature =
    { name : string
    ; typeAnnotation : TypeAnnotation
    }

type FunctionImplementation =
    { name : string
    ; arguments : List<Pattern>
    ; expression : Expression
    }

and Function =
    { documentation : Option<Documentation>
    ; declaration : FunctionImplementation
    }

and Case =
    (Pattern * Expression)

and Cases =
    List<Case>

and CaseBlock =
    { expression : Expression
    ; cases : Cases
    }

and RecordSetter =
    ( string * Expression )

and LetDeclaration
    = LetFunction of Function
    | LetDestructuring of Pattern * Expression

and LetBlock =
    { declarations : List<LetDeclaration>
    ; expression : Expression
    }

and Lambda =
    { args : List<Pattern>
    ; expression : Expression
    }

and Expression
    = UnitExpr
    | Application of List<Expression>
    | OperatorApplication of
          string *
          InfixDirection *
          Expression *
          Expression
    | FunctionOrValue of ModuleName * string
    | IfBlock of Expression * Expression * Expression
    | PrefixOperator of string
    | Operator of string
    | Integer of int64
    | Hex of int64
    | Floatable of double
    | Negation of Expression
    | Literal of string
    | CharLiteral of char
    | TupledExpression of List<Expression>
    | ParenthesizedExpression of Expression
    | LetExpression of LetBlock
    | CaseExpression of CaseBlock
    | LambdaExpression of Lambda
    | RecordExpr of List<RecordSetter>
    | ListExpr of List<Expression>
    | RecordAccess of Expression * string
    | RecordAccessFunction of string
    | RecordUpdateExpression of string * List<RecordSetter>
    | GLSLExpression of string

type IsExposed = bool

type Declaration
    = FunctionDeclaration of Function * IsExposed
    | PortDeclaration of Signature * IsExposed
    | InfixDeclaration of Infix * IsExposed
    | Destructuring of Pattern * Expression * IsExposed

type TypeDeclaration
    = AliasDeclaration of TypeAlias * IsExposed
    | CustomTypeDeclaration of Type * IsExposed

type Comment =
    string

type File = 
    { moduleDefinition: ModuleName
    ; typeDeclarations: TypeDeclaration List
    ; declarations: Declaration List
    ; comments : Comment List
    }

let nodeValue<'a> (node: 'a ElmAst.Node): 'a =
    let (_, value) = node
    value

let moduleDefinitionName (moduleDefinition: ElmAst.Module): ModuleName = 
    match moduleDefinition with
    | ElmAst.NormalModule a -> a.moduleName |> nodeValue
    | ElmAst.EffectModule a -> a.moduleName |> nodeValue
    | ElmAst.PortModule a -> a.moduleName |> nodeValue

let moduleExposings (moduleDefinition: ElmAst.Module): ElmAst.Exposing = 
    match moduleDefinition with
    | ElmAst.NormalModule a -> a.exposingList |> nodeValue
    | ElmAst.EffectModule a -> a.exposingList |> nodeValue
    | ElmAst.PortModule a -> a.exposingList |> nodeValue

let fromInfixDirection (infixDirection: ElmAst.InfixDirection): InfixDirection =
    match infixDirection with
    | ElmAst.Left -> Left
    | ElmAst.Right -> Right
    | ElmAst.Non -> Non

let fromQualifiedNameRef (qualifiedNameRef: ElmAst.QualifiedNameRef): QualifiedNameRef =
    { moduleName = qualifiedNameRef.moduleName
    ; name = qualifiedNameRef.name
    }

let rec fromPattern (pattern: ElmAst.Pattern): Pattern =
    match pattern with
    | ElmAst.AllPattern -> AllPattern
    | ElmAst.UnitPattern -> UnitPattern
    | ElmAst.CharPattern a -> CharPattern a
    | ElmAst.StringPattern a -> StringPattern a
    | ElmAst.IntPattern a -> IntPattern a
    | ElmAst.HexPattern a -> HexPattern a
    | ElmAst.FloatPattern a -> FloatPattern a
    | ElmAst.TuplePattern a -> a |> List.map (nodeValue >> fromPattern) |> TuplePattern
    | ElmAst.RecordPattern a -> a |> List.map nodeValue |> RecordPattern
    | ElmAst.UnConsPattern (a, b) -> 
        UnConsPattern
            ( a |> nodeValue |> fromPattern
            , b |> nodeValue |> fromPattern
            )
    | ElmAst.ListPattern a -> a |> List.map (nodeValue >> fromPattern) |> ListPattern
    | ElmAst.VarPattern a -> VarPattern a
    | ElmAst.NamedPattern (a, b) -> 
        NamedPattern
            ( fromQualifiedNameRef a
            , b |> List.map (nodeValue >> fromPattern)
            )
    | ElmAst.AsPattern (a, b) -> 
        AsPattern 
            ( a |> nodeValue |> fromPattern
            , b |> nodeValue
            ) 
    | ElmAst.ParenthesizedPattern a -> a |> nodeValue |> fromPattern |> ParenthesizedPattern

and fromExpression (expression: ElmAst.Expression): Expression = 
    match expression with
    | ElmAst.UnitExpr -> UnitExpr
    | ElmAst.Application a -> a |> List.map (nodeValue >> fromExpression) |> Application
    | ElmAst.OperatorApplication (a, b, c, d) -> 
        OperatorApplication 
            ( a
            , fromInfixDirection b
            , c |> nodeValue |> fromExpression
            , d |> nodeValue |> fromExpression
            )
    | ElmAst.FunctionOrValue (a, b) -> FunctionOrValue (a, b)
    | ElmAst.IfBlock (ifClause, thenClause, elseClause) -> 
        IfBlock 
            ( ifClause |> nodeValue |> fromExpression
            , thenClause |> nodeValue |> fromExpression
            , elseClause |> nodeValue |> fromExpression
            )
    | ElmAst.PrefixOperator a -> PrefixOperator a
    | ElmAst.Operator a -> Operator a
    | ElmAst.Integer value -> Integer value
    | ElmAst.Hex value -> Hex value
    | ElmAst.Floatable value -> Floatable value
    | ElmAst.Negation a -> a |> nodeValue |> fromExpression |> Negation
    | ElmAst.Literal a -> Literal a
    | ElmAst.CharLiteral a -> CharLiteral a
    | ElmAst.TupledExpression a -> a |> List.map (nodeValue >> fromExpression) |> TupledExpression
    | ElmAst.ParenthesizedExpression a -> a |> nodeValue |> fromExpression |> ParenthesizedExpression
    | ElmAst.LetExpression a -> a |> fromLetBlock |> LetExpression
    | ElmAst.CaseExpression a -> a |> fromCaseBlock |> CaseExpression
    | ElmAst.LambdaExpression a -> a |> fromLambda |> LambdaExpression
    | ElmAst.RecordExpr a -> a |> List.map (nodeValue >> fromRecordSetter) |> RecordExpr
    | ElmAst.ListExpr a -> a |> List.map (nodeValue >> fromExpression) |> ListExpr
    | ElmAst.RecordAccess (a, b) -> 
        RecordAccess 
            ( a |> nodeValue |> fromExpression
            , b |> nodeValue
            )
    | ElmAst.RecordAccessFunction a -> RecordAccessFunction a
    | ElmAst.RecordUpdateExpression (a, b) -> 
        RecordUpdateExpression
            ( a |> nodeValue
            , b |> List.map (nodeValue >> fromRecordSetter)
            )
    | ElmAst.GLSLExpression a -> GLSLExpression a

and fromRecordSetter ((fieldName, expression): ElmAst.RecordSetter): RecordSetter = 
    ( nodeValue fieldName
    , expression |> nodeValue |> fromExpression
    )

and fromLambda (lambda: ElmAst.Lambda): Lambda = 
    { args = lambda.args |> List.map (nodeValue >> fromPattern)
    ; expression = lambda.expression |> nodeValue |> fromExpression
    }

and fromCase ((pattern, expression): ElmAst.Case): Case =
    (pattern |> nodeValue |> fromPattern
    , expression |> nodeValue |> fromExpression
    )

and fromCaseBlock (caseBlock: ElmAst.CaseBlock): CaseBlock = 
    { expression = caseBlock.expression |> nodeValue |> fromExpression
    ; cases = caseBlock.cases |> List.map fromCase
    }

and fromLetDeclaration (letDeclaration: ElmAst.LetDeclaration): LetDeclaration = 
    match letDeclaration with
    | ElmAst.LetFunction a -> a |> fromFunction |> LetFunction
    | ElmAst.LetDestructuring (pattern, expression) -> 
        LetDestructuring 
            ( pattern |> nodeValue |> fromPattern
            , expression |> nodeValue |> fromExpression
            )

and fromLetBlock (letExpression: ElmAst.LetBlock): LetBlock =
    { declarations = letExpression.declarations |> List.map (nodeValue >> fromLetDeclaration)
    ; expression = letExpression.expression |> nodeValue |> fromExpression
    }

and fromFunctionImplementation (functionImplementation: ElmAst.FunctionImplementation): FunctionImplementation =
    { name = functionImplementation.name |> nodeValue
    ; arguments = []
    ; expression = functionImplementation.expression |> nodeValue |> fromExpression
    }

and fromFunction (``function``: ElmAst.Function): Function =
    { documentation = ``function``.documentation |> Option.map nodeValue
    ; declaration = ``function``.declaration |> nodeValue |> fromFunctionImplementation
    }

let rec fromTypeAnnotation (typeAnnotation: ElmAst.TypeAnnotation): TypeAnnotation =
    match typeAnnotation with
    | ElmAst.GenericType a -> GenericType a
    | ElmAst.Typed (a, b) -> 
        Typed 
            ( nodeValue a
            , b |> List.map (nodeValue >> fromTypeAnnotation)
            )
    | ElmAst.Unit -> Unit
    | ElmAst.Tupled a -> a |> List.map (nodeValue >> fromTypeAnnotation) |> Tupled 
    | ElmAst.Record a -> a |> fromRecordDefinition |> Record
    | ElmAst.GenericRecord (a, b) -> 
        GenericRecord
            ( nodeValue a
            , b |> nodeValue |> fromRecordDefinition
            )
    | ElmAst.FunctionTypeAnnotation (a, b) -> 
        FunctionTypeAnnotation 
            ( a |> nodeValue |> fromTypeAnnotation
            , b |> nodeValue |> fromTypeAnnotation
            )
 
and fromRecordField ((fieldName, typeAnnotation): ElmAst.RecordField): RecordField =
    (nodeValue fieldName, typeAnnotation |> nodeValue |> fromTypeAnnotation)

and fromRecordDefinition (recordDefinition: ElmAst.RecordDefinition): RecordDefinition = 
    recordDefinition |> List.map (nodeValue >> fromRecordField)

let fromTypeAlias (alias: ElmAst.TypeAlias): TypeAlias =
    { documentation = alias.documentation |> Option.map nodeValue
    ; name = alias.name |> nodeValue
    ; generics = alias.generics |> List.map nodeValue
    ; typeAnnotation = alias.typeAnnotation |> nodeValue |> fromTypeAnnotation
    }

let fromCustomType (customType: ElmAst.Type): Type =
    { documentation = customType.documentation |> Option.map nodeValue
    ; name = customType.name |> nodeValue
    ; generics = customType.generics |> List.map nodeValue
    ; constructors = []
    }

let isExposed (exposing: ElmAst.Exposing) (declaration: ElmAst.Declaration) =
    match exposing with
    | ElmAst.All _ -> true
    | ElmAst.Explicit a -> 
        let exposeList = a |> List.map nodeValue
        match declaration with
        | ElmAst.FunctionDeclaration b -> 
            List.contains 
                (b.declaration 
                |> nodeValue 
                |> (fun a -> a.name) 
                |> nodeValue 
                |> ElmAst.FunctionExpose)
                exposeList
        | ElmAst.AliasDeclaration b -> 
            List.contains 
                (b.name |> nodeValue |> ElmAst.TypeOrAliasExpose)
                exposeList
        | ElmAst.CustomTypeDeclaration b -> 
            let name = nodeValue b.name
            List.exists 
                (fun item -> 
                    match item with
                    | ElmAst.TypeOrAliasExpose text -> text = name
                    | ElmAst.TypeExpose typeExpose -> typeExpose.name = name
                    | _ -> false
                )
                exposeList
        | ElmAst.PortDeclaration b -> raise (new NotImplementedException())
        | ElmAst.InfixDeclaration b -> raise (new NotImplementedException())
        | ElmAst.Destructuring (b, c) -> raise (new NotImplementedException())

let fromDeclarations (exposing: ElmAst.Exposing) (declarations: ElmAst.Declaration List): (TypeDeclaration List * Declaration List) =
    List.fold 
        (fun (typeDeclarations, functionDeclarations) a -> 
            match a with
            | ElmAst.FunctionDeclaration b -> 
                let newDeclaration = FunctionDeclaration (fromFunction b, isExposed exposing a)
                ( typeDeclarations
                , newDeclaration :: functionDeclarations
                )
            | ElmAst.AliasDeclaration b -> 
                let newDeclaration = AliasDeclaration (fromTypeAlias b, isExposed exposing a)
                ( newDeclaration :: typeDeclarations
                , functionDeclarations
                )
            | ElmAst.CustomTypeDeclaration b -> 
                let newDeclaration = CustomTypeDeclaration (fromCustomType b, isExposed exposing a)
                ( newDeclaration :: typeDeclarations
                , functionDeclarations
                )
            | ElmAst.PortDeclaration b -> raise (new NotImplementedException())
            | ElmAst.InfixDeclaration b -> raise (new NotImplementedException())
            | ElmAst.Destructuring (b, c) -> raise (new NotImplementedException())
        )
        ([], [])
        declarations
    

let fromFile (file: ElmAst.File): File =
    let ``module`` = file.moduleDefinition |> nodeValue
    let (typeDeclarations, declarations) = 
        fromDeclarations 
            (moduleExposings ``module``) 
            (List.map nodeValue file.declarations)
    { moduleDefinition = moduleDefinitionName ``module``
    ; typeDeclarations = typeDeclarations
    ; declarations = declarations
    ; comments = file.comments |> List.map nodeValue
    }