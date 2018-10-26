module Ast

type ModuleName =
    List<string>

//type Location =
//    { row : int
//    ; column : int
//    }

//type Range =
//    { start : Location
//    ; ``end`` : Location
//    }

//type ExposedType =
//    { name : string
//    ; ``open`` : Option<Range>
//    }

//type TopLevelExpose
//    = InfixExpose of string
//    | FunctionExpose of string
//    | TypeOrAliasExpose of string
//    | TypeExpose of ExposedType

//type Exposing
//    = All of Range
//    | Explicit of List<TopLevelExpose>

//type DefaultModuleData =
//    { moduleName : ModuleName
//    ; exposingList : Exposing
//    }

//type EffectModuleData =
//    { moduleName : ModuleName
//    ; exposingList : Exposing
//    ; command : Option<string>
//    ; subscription : Option<string>
//    }

//type Module
//    = NormalModule of DefaultModuleData
//    | PortModule of DefaultModuleData
//    | EffectModule of EffectModuleData

//type Import =
//    { moduleName : ModuleName
//    ; moduleAlias : Option<ModuleName>
//    ; exposingList : Option<Exposing>
//    }

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
    ; signature : Option<Signature>
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

type Declaration
    = FunctionDeclaration of Function
    | AliasDeclaration of TypeAlias
    | CustomTypeDeclaration of Type
    | PortDeclaration of Signature
    | InfixDeclaration of Infix
    | Destructuring of Pattern * Expression

type Comment =
    string

type File = 
    { moduleDefinition: ModuleName
    ; declarations: List<Declaration>
    ; comments : List<Comment>
    }

let nodeValue<'a> (node: 'a ElmAst.Node): 'a =
    let (_, value) = node
    value

let moduleDefinitionName (moduleDefinition: ElmAst.Module): ModuleName = 
    match moduleDefinition with
    | ElmAst.NormalModule a -> a.moduleName |> nodeValue
    | ElmAst.EffectModule a -> a.moduleName |> nodeValue
    | ElmAst.PortModule a -> a.moduleName |> nodeValue

let transform (file: ElmAst.File): File =
    { moduleDefinition = file.moduleDefinition |> nodeValue |> moduleDefinitionName
    ; declarations = []
    ; comments = file.comments |> List.map nodeValue
    }