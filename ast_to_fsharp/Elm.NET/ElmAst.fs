module ElmAst

type ModuleName =
    List<string>

type Location =
    { row : int
    ; column : int
    }

type Range =
    { start : Location
    ; ``end`` : Location
    }

type Node<'a>
    = Range * 'a

type ExposedType =
    { name : string
    ; ``open`` : Option<Range>
    }

type TopLevelExpose
    = InfixExpose of string
    | FunctionExpose of string
    | TypeOrAliasExpose of string
    | TypeExpose of ExposedType

type Exposing
    = All of Range
    | Explicit of List<Node<TopLevelExpose>>

type DefaultModuleData =
    { moduleName : Node<ModuleName>
    ; exposingList : Node<Exposing>
    }

type EffectModuleData =
    { moduleName : Node<ModuleName>
    ; exposingList : Node<Exposing>
    ; command : Option<Node<string>>
    ; subscription : Option<Node<string>>
    }

type Module
    = NormalModule of DefaultModuleData
    | PortModule of DefaultModuleData
    | EffectModule of EffectModuleData

type Import =
    { moduleName : Node<ModuleName>
    ; moduleAlias : Option<Node<ModuleName>>
    ; exposingList : Option<Node<Exposing>>
    }

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
    | TuplePattern of List<Node<Pattern>>
    | RecordPattern of List<Node<string>>
    | UnConsPattern of Node<Pattern> * Node<Pattern>
    | ListPattern of List<Node<Pattern>>
    | VarPattern of string
    | NamedPattern of QualifiedNameRef * List<Node<Pattern>>
    | AsPattern of Node<Pattern> * Node<string>
    | ParenthesizedPattern of Node<Pattern>

type Documentation =
    string

type RecordField =
    ( Node<string> * Node<TypeAnnotation> )

and RecordDefinition =
        List<Node<RecordField>>

and TypeAnnotation
        = GenericType of string
        | Typed of Node<(ModuleName * string)> * List<Node<TypeAnnotation>>
        | Unit
        | Tupled of List<Node<TypeAnnotation>>
        | Record of RecordDefinition
        | GenericRecord of Node<string> * Node<RecordDefinition>
        | FunctionTypeAnnotation of Node<TypeAnnotation> * Node<TypeAnnotation>

type ValueConstructor =
    { name : Node<string>
    ; arguments : List<Node<TypeAnnotation>>
    }

type Type =
    { documentation : Option<Node<Documentation>>
    ; name : Node<string>
    ; generics : List<Node<string>>
    ; constructors : List<Node<ValueConstructor>>
    }

type TypeAlias =
    { documentation : Option<Node<Documentation>>
    ; name : Node<string>
    ; generics : List<Node<string>>
    ; typeAnnotation : Node<TypeAnnotation>
    }

type InfixDirection
    = Left
    | Right
    | Non

type Infix =
    { direction : Node<InfixDirection>
    ; precedence : Node<int>
    ; operator : Node<string>
    ; ``function`` : Node<string>
    }

type Signature =
    { name : Node<string>
    ; typeAnnotation : Node<TypeAnnotation>
    }

type FunctionImplementation =
    { name : Node<string>
    ; arguments : List<Node<Pattern>>
    ; expression : Node<Expression>
    }

and Function =
    { documentation : Option<Node<Documentation>>
    ; signature : Option<Node<Signature>>
    ; declaration : Node<FunctionImplementation>
    }

and Case =
    (Node<Pattern> * Node<Expression>)

and Cases =
    List<Case>

and CaseBlock =
    { expression : Node<Expression>
    ; cases : Cases
    }

and RecordSetter =
    ( Node<string> * Node<Expression> )

and LetDeclaration
    = LetFunction of Function
    | LetDestructuring of Node<Pattern> * Node<Expression>

and LetBlock =
    { declarations : List<Node<LetDeclaration>>
    ; expression : Node<Expression>
    }

and Lambda =
    { args : List<Node<Pattern>>
    ; expression : Node<Expression>
    }

and Expression
    = UnitExpr
    | Application of List<Node<Expression>>
    | OperatorApplication of
          string *
          InfixDirection *
          Node<Expression> *
          Node<Expression>
    | FunctionOrValue of ModuleName * string
    | IfBlock of Node<Expression> * Node<Expression> * Node<Expression>
    | PrefixOperator of string
    | Operator of string
    | Integer of int64
    | Hex of int64
    | Floatable of double
    | Negation of Node<Expression>
    | Literal of string
    | CharLiteral of char
    | TupledExpression of List<Node<Expression>>
    | ParenthesizedExpression of Node<Expression>
    | LetExpression of LetBlock
    | CaseExpression of CaseBlock
    | LambdaExpression of Lambda
    | RecordExpr of List<Node<RecordSetter>>
    | ListExpr of List<Node<Expression>>
    | RecordAccess of Node<Expression> * Node<string>
    | RecordAccessFunction of string
    | RecordUpdateExpression of Node<string> * List<Node<RecordSetter>>
    | GLSLExpression of string

type Declaration
    = FunctionDeclaration of Function
    | AliasDeclaration of TypeAlias
    | CustomTypeDeclaration of Type
    | PortDeclaration of Signature
    | InfixDeclaration of Infix
    | Destructuring of Node<Pattern> * Node<Expression>

type Comment =
    string

type File = 
    { moduleDefinition: Node<Module>
    ; imports: List<Node<Import>>
    ; declarations: List<Node<Declaration>>
    ; comments : List<Node<Comment>> 
    }