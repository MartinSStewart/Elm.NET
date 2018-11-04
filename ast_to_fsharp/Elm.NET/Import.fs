module Import

open Newtonsoft.Json
open ElmAst
open Newtonsoft.Json.Linq
open System
open Helper


let decodeRange (json : JArray): Range = 
    let values = decodeList int json
    { start = { row = values.[0]; column = values.[1] }
    ; ``end`` = { row = values.[2]; column = values.[3] }
    }

let decodeNode<'a, 'b> (decoder : 'b -> 'a) (json : JObject): Node<'a> =
    ( json.Value<JArray>("range") |> decodeRange
    , json.Value<'b>("value") |> decoder
    )

let decodeModuleName (json : JArray): ModuleName =
    decodeList string json

let decodeExposedType (json : JObject): ExposedType =
    { name = json.Value<string>("name")
    ; ``open`` = json.Value<JArray>("open") |> decodeOption decodeRange 
    }

let decodeTopLevelExpose (json : JObject): TopLevelExpose =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "infix" -> 
        json.Value<string>("infix") 
        |> InfixExpose
    | "function" -> 
        json.Value<JObject>("function") 
        |> (fun a -> a.Value<string>("name"))
        |> FunctionExpose
    | "typeOrAlias" -> 
        json.Value<JObject>("typeOrAlias")  
        |> (fun a -> a.Value<JToken>("name"))
        |> decodeString
        |> TypeOrAliasExpose
    | "typeexpose" -> 
        json.Value<JObject>("typeexpose") 
        |> decodeExposedType
        |> TypeExpose
    | _ -> raise (new NotImplementedException())

let decodeExposing (json : JObject): Exposing =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "all" -> 
        json.Value<JArray>("all") 
        |> decodeRange 
        |> All
    | "explicit" -> 
        json.Value<JArray>("explicit") 
        |> decodeList (decodeNode decodeTopLevelExpose)
        |> Explicit
    | _ -> raise (new NotImplementedException())

let decodeDefaultModuleData (json : JObject): DefaultModuleData =
    { moduleName = decodeNode decodeModuleName (json.Value<JObject>("moduleName"))
    ; exposingList = decodeNode decodeExposing (json.Value<JObject>("exposingList"))
    }

let decodeEffectModuleData (json : JObject): EffectModuleData =
    { moduleName = decodeNode decodeModuleName (json.Value<JObject>("moduleName"))
    ; exposingList = decodeNode decodeExposing (json.Value<JObject>("exposingList"))
    ; command = raise (new NotImplementedException())
    ; subscription = raise (new NotImplementedException())
    }

let decodeModule (json : JObject): Module =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "normal" ->
        json.Value<JObject>("normal") |> decodeDefaultModuleData |> NormalModule
    | "port" ->
        json.Value<JObject>("port") |> decodeDefaultModuleData |> PortModule
    | "effect" -> 
        json.Value<JObject>("effect") |> decodeEffectModuleData |> EffectModule
    | _ -> raise (new NotImplementedException())


let decodeModuleDefinition (json : JObject): Node<Module> =
    decodeNode decodeModule json

let decodeImport (json : JObject): Import =
    { moduleName = json.Value<JObject>("moduleName") |> decodeNode decodeModuleName
    ; moduleAlias = json.Value<JObject>("moduleAlias") |> decodeOption (decodeNode decodeModuleName)
    ; exposingList = json.Value<JObject>("exposingList") |> decodeOption (decodeNode decodeExposing)
    }

let decodeDocumentation (json : JToken): Documentation =
    decodeString json

let decodeModuleNameAndName (json : JToken): ModuleName * string =
    ( json.Value<JArray>("moduleName") |> decodeModuleName
    , json.Value<JToken>("name") |> decodeString
    )

let rec decodeRecordField (json : JObject): RecordField =
    ( json.Value<JObject>("name") |> decodeNode decodeString
    , json.Value<JObject>("typeAnnotation") |> decodeNode decodeTypeAnnotation
    )

and decodeRecordDefinition (json : JObject): RecordDefinition =
    json.Value<JArray>("value") |> decodeList (decodeNode decodeRecordField)

and decodeTypeAnnotation (json : JObject): TypeAnnotation =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "generic" ->
        json.Value<JObject>("generic") 
        |> (fun a -> a.Value<JToken>("value")) 
        |> decodeString 
        |> GenericType 
    | "typed" ->
        let typedJson = json.Value<JObject>("typed") 
        ( typedJson.Value<JObject>("moduleNameAndName") |> decodeNode decodeModuleNameAndName
        , typedJson.Value<JArray>("args") |> decodeList (decodeNode decodeTypeAnnotation)
        ) 
        |> Typed
    | "unit" -> 
        Unit
    | "tupled" ->
        json.Value<JObject>("tupled") 
        |> (fun a -> a.Value<JArray>("values")) 
        |> decodeList (decodeNode decodeTypeAnnotation) 
        |> Tupled
    | "record" -> 
        json.Value<JObject>("record") |> decodeRecordDefinition |> Record
    //| "genericRecord" -> 
    //    json.Value<JObject>("genericRecord") |> decodeEffectModuleData |> GenericRecord
    | "function" -> 
        let functionJson = json.Value<JObject>("function")
        ( functionJson.Value<JObject>("left") |> decodeNode decodeTypeAnnotation
        , functionJson.Value<JObject>("right") |> decodeNode decodeTypeAnnotation
        )
        |> FunctionTypeAnnotation
    | _ -> raise (new NotImplementedException())

let decodeSignature (json : JObject): Signature =
    { name = json.Value<JObject>("name") |> decodeNode decodeString
    ; typeAnnotation = json.Value<JObject>("typeAnnotation") |> decodeNode decodeTypeAnnotation
    }

let decodeChar (json : JToken): char =
    (decodeString json).[0]

let decodeInt (json : JToken): int64 =
    json.Value<int64>()

let decodeHex (json : JToken): int64 =
    decodeInt json

let decodeDouble (json : JToken): double =
    json.Value<double>()

let decodeQualifiedNameRef (json : JObject): QualifiedNameRef =
    { moduleName = json.Value<JArray>("moduleName") |> decodeList decodeString
    ; name = json.Value<JToken>("name") |> decodeString 
    }

let rec decodePattern (json : JObject): Pattern =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "all" -> AllPattern
    | "unit" -> UnitPattern
    | "char" -> json.Value<JToken>("char") |> decodeChar |> CharPattern
    | "string" -> json.Value<JToken>("string") |> decodeString |> StringPattern
    | "hex" -> json.Value<JToken>("hex") |> decodeHex |> HexPattern
    | "int" -> json.Value<JToken>("int") |> decodeInt |> IntPattern
    | "float" -> json.Value<JToken>("float") |> decodeDouble |> FloatPattern
    | "tuple" -> 
        json.Value<JToken>("tuple") 
        |> (fun a -> a.Value<JArray>("value")) 
        |> decodeList (decodeNode decodePattern) 
        |> TuplePattern
    | "record" -> 
        json.Value<JObject>("record") 
        |> (fun a -> a.Value<JArray>("value")) 
        |> decodeList (decodeNode decodeString) 
        |> RecordPattern
    //| "uncons" -> json.Value<JObject>("uncons") |> decodeList (decodeNode decodeString) |> UnConsPattern
    | "list" -> json.Value<JArray>("list") |> decodeList (decodeNode decodePattern) |> ListPattern
    | "var" -> json.Value<JToken>("var") |> (fun varJson -> varJson.Value<JToken>("value")) |> decodeString |> VarPattern
    | "named" -> 
        let namedJson = json.Value<JObject>("named")
        ( namedJson.Value<JObject>("qualified") |> decodeQualifiedNameRef
        , namedJson.Value<JArray>("patterns") |> decodeList (decodeNode decodePattern)
        )
        |> NamedPattern
    //| "as" -> AsPattern of Node<Pattern> * Node<string>
    | "parentisized" -> 
        json.Value<JObject>("parentisized") 
        |> (fun a -> a.Value<JObject>("value")) 
        |> decodeNode decodePattern 
        |> ParenthesizedPattern
    | _ -> raise (new NotImplementedException())

let decodeInfixDirection (json : JToken): InfixDirection =
    match decodeString json with
    | "left" -> Left
    | "right" -> Right
    | "non" -> Non
    | _ -> raise (new NotImplementedException())

let rec decodeExpression (json : JObject): Expression =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "unit" -> UnitExpr
    | "application" -> json.Value<JArray>("application") |> decodeList (decodeNode decodeExpression) |> Application
    | "operatorapplication" -> 
        let operatorApplicationJson = json.Value<JObject>("operatorapplication")
        ( operatorApplicationJson.Value<JToken>("operator") |> decodeString
        , operatorApplicationJson.Value<JToken>("direction") |> decodeInfixDirection
        , operatorApplicationJson.Value<JObject>("left") |> decodeNode decodeExpression
        , operatorApplicationJson.Value<JObject>("right") |> decodeNode decodeExpression
        )
        |> OperatorApplication
    | "functionOrValue" -> 
        let functionOrValueJson = json.Value<JObject>("functionOrValue")
        ( functionOrValueJson.Value<JArray>("moduleName") |> decodeModuleName
        , functionOrValueJson.Value<JToken>("name") |> decodeString
        )
        |> FunctionOrValue
    | "ifBlock" -> 
        let ifBlockJson = json.Value<JObject>("ifBlock")
        ( ifBlockJson.Value<JObject>("clause") |> decodeNode decodeExpression
        , ifBlockJson.Value<JObject>("then") |> decodeNode decodeExpression
        , ifBlockJson.Value<JObject>("else") |> decodeNode decodeExpression
        )
        |> IfBlock
    | "prefixoperator" -> json.Value<JToken>("prefixoperator") |> decodeString |> PrefixOperator
    | "operator" -> json.Value<JToken>("operator") |> decodeString |> Operator
    | "integer" -> json.Value<JToken>("integer") |> decodeInt |> Integer
    | "hex" -> json.Value<JToken>("hex") |> decodeHex |> Hex
    | "float" -> json.Value<JToken>("float") |> decodeDouble |> Floatable
    | "negation" -> json.Value<JObject>("negation") |> decodeNode decodeExpression |> Negation
    | "literal" -> json.Value<JToken>("literal") |> decodeString |> Literal
    | "charLiteral" -> json.Value<JToken>("charLiteral") |> decodeChar |> CharLiteral
    | "tupled" -> json.Value<JArray>("tupled") |> decodeList (decodeNode decodeExpression) |> TupledExpression
    | "parenthesized" -> json.Value<JObject>("parenthesized") |> decodeNode decodeExpression |> ParenthesizedExpression
    | "let" -> json.Value<JObject>("let") |> decodeLetBlock |> LetExpression
    | "case" -> json.Value<JObject>("case") |> decodeCaseBlock |> CaseExpression
    | "lambda" -> json.Value<JObject>("lambda") |> decodeLambda |> LambdaExpression
    | "recordAccess" -> 
        let recordAccessJson = json.Value<JObject>("recordAccess")
        ( recordAccessJson.Value<JObject>("expression") |> decodeNode decodeExpression
        , recordAccessJson.Value<JObject>("name") |> decodeNode decodeString
        )
        |> RecordAccess
    | "list" -> json.Value<JArray>("list") |> decodeList (decodeNode decodeExpression) |> ListExpr
    | "record" -> json.Value<JArray>("record") |> decodeList (decodeNode decodeRecordSetter) |> RecordExpr
    | "recordAccessFunction" -> json.Value<JToken>("recordAccessFunction") |> decodeString |> RecordAccessFunction
    | "recordUpdate" -> 
        let recordUpdateJson = json.Value<JObject>("recordUpdate") 
        ( recordUpdateJson.Value<JObject>("name") |> decodeNode decodeString
        , recordUpdateJson.Value<JArray>("updates") |> decodeList (decodeNode decodeRecordSetter)
        )
        |> RecordUpdateExpression
    | "glsl" -> json.Value<JObject>("glsl") |> decodeString |> GLSLExpression
    | _ -> raise (new NotImplementedException())

and decodeLetDeclaration (json : JObject): LetDeclaration =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "function" -> json.Value<JObject>("function") |> decodeFunction |> LetFunction
    | "destructuring" -> 
        let destructuringJson = json.Value<JObject>("destructuring")
        ( destructuringJson.Value<JObject>("pattern") |> decodeNode decodePattern
        , destructuringJson.Value<JObject>("expression") |> decodeNode decodeExpression
        )
        |> LetDestructuring
    | _ -> raise (new NotImplementedException())

and decodeLetBlock (json : JObject): LetBlock =
    { declarations = json.Value<JArray>("declarations") |> decodeList (decodeNode decodeLetDeclaration)
    ; expression = json.Value<JObject>("expression") |> decodeNode decodeExpression
    }

and decodeFunctionImplementation (json : JObject): FunctionImplementation =
    { name = json.Value<JObject>("name") |> decodeNode decodeString
    ; arguments = json.Value<JArray>("arguments") |> decodeList (decodeNode decodePattern)
    ; expression = json.Value<JObject>("expression") |> decodeNode decodeExpression
    }

and decodeFunction (json : JObject): Function =
    { documentation = json.Value<JObject>("documentation") |> decodeOption (decodeNode decodeDocumentation)
    ; signature = json.Value<JObject>("signature") |> decodeOption (decodeNode decodeSignature)
    ; declaration = json.Value<JObject>("declaration") |> decodeNode decodeFunctionImplementation
    }

and decodeCase (json : JObject): Case =
    ( json.Value<JObject>("pattern") |> decodeNode decodePattern
    , json.Value<JObject>("expression") |> decodeNode decodeExpression
    )

and decodeCases (json : JArray): Cases =
    decodeList decodeCase json

and decodeCaseBlock (json : JObject): CaseBlock =
    { expression = json.Value<JObject>("expression") |> decodeNode decodeExpression
    ; cases = json.Value<JArray>("cases") |> decodeCases
    }

and decodeLambda (json : JObject): Lambda =
    { args = json.Value<JArray>("patterns") |> decodeList (decodeNode decodePattern)
    ; expression = json.Value<JObject>("expression") |> decodeNode decodeExpression
    }

and decodeRecordSetter (json : JObject): RecordSetter =
    ( json.Value<JObject>("field") |> decodeNode decodeString
    , json.Value<JObject>("expression") |> decodeNode decodeExpression
    )

let decodeTypeAlias (json : JObject): TypeAlias =
    { documentation = json.Value<JObject>("documentation") |> decodeOption (decodeNode decodeDocumentation)
    ; name = json.Value<JObject>("name") |> decodeNode decodeString
    ; generics = json.Value<JArray>("generics") |> decodeList (decodeNode decodeString)
    ; typeAnnotation = json.Value<JObject>("typeAnnotation") |> decodeNode decodeTypeAnnotation
    }

let decodeValueConstructor (json : JObject): ValueConstructor =
    { name = json.Value<JObject>("name") |> decodeNode decodeString
    ; arguments = json.Value<JArray>("arguments") |> decodeList (decodeNode decodeTypeAnnotation)
    }

let decodeType (json : JObject): ElmAst.Type =
    { documentation = json.Value<JObject>("documentation") |> decodeOption (decodeNode decodeDocumentation)
    ; name = json.Value<JObject>("name") |> decodeNode decodeString
    ; generics = json.Value<JArray>("generics") |> decodeList (decodeNode decodeString)
    ; constructors = json.Value<JArray>("constructors") |> decodeList (decodeNode decodeValueConstructor)
    }

let decodeInfix (json : JObject): Infix =
    raise (new NotImplementedException())

let decodeDeclaration (json : JObject): Declaration =
    let moduleType = json.Value<string>("type")
    match moduleType with
    | "function" ->
        json.Value<JObject>("function") |> decodeFunction |> FunctionDeclaration
    | "typeAlias" ->
        json.Value<JObject>("typeAlias") |> decodeTypeAlias |> AliasDeclaration
    | "typedecl" -> 
        json.Value<JObject>("typedecl") |> decodeType |> CustomTypeDeclaration
    | "port" -> 
        json.Value<JObject>("port") |> decodeSignature |> PortDeclaration
    | "infix" -> 
        json.Value<JObject>("infix") |> decodeInfix |> InfixDeclaration
    //| "destructuring" -> 
    //    json.Value<JObject>("destructuring") |> decodeEffectModuleData |> Destructuring
    | _ -> raise (new NotImplementedException())

let decodeFile (json : JObject): File =
    { moduleDefinition = json.Value<JObject>("moduleDefinition") |> decodeModuleDefinition
    ; imports = json.Value<JArray>("imports") |> decodeList (decodeNode decodeImport)
    ; declarations = json.Value<JArray>("declarations") |> decodeList (decodeNode decodeDeclaration)
    ; comments = []
    }

// Loads the json containing the Elm AST and parses it.
let import (text: string):File List =
    JArray.Parse(text) |> decodeList decodeFile