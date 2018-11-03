module Helper

open System
open System.Numerics

[<AutoOpen>]
module NumericLiteralG = 
    type GenericNumber = GenericNumber with
        static member inline genericNumber (x:int32, _:int8) = int8 x
        static member inline genericNumber (x:int32, _:uint8) = uint8 x
        static member inline genericNumber (x:int32, _:int16) = int16 x
        static member inline genericNumber (x:int32, _:uint16) = uint16 x
        static member inline genericNumber (x:int32, _:int32) = x
        static member inline genericNumber (x:int32, _:uint32) = uint32 x
        static member inline genericNumber (x:int32, _:int64) = int64 x
        static member inline genericNumber (x:int32, _:uint64) = uint64 x
        static member inline genericNumber (x:int32, _:float32) = float32 x
        static member inline genericNumber (x:int32, _:float) = float x
        static member inline genericNumber (x:int32, _:bigint) = bigint x
        static member inline genericNumber (x:int32, _:decimal) = decimal x
        static member inline genericNumber (x:int32, _:Complex) = Complex.op_Implicit x
        static member inline genericNumber (x:int64, _:int64) = int64 x
        static member inline genericNumber (x:int64, _:uint64) = uint64 x
        static member inline genericNumber (x:int64, _:float32) = float32 x
        static member inline genericNumber (x:int64, _:float) = float x
        static member inline genericNumber (x:int64, _:bigint) = bigint x
        static member inline genericNumber (x:int64, _:decimal) = decimal x
        static member inline genericNumber (x:int64, _:Complex) = Complex.op_Implicit x
        static member inline genericNumber (x:string, _:float32) = float32 x
        static member inline genericNumber (x:string, _:float) = float x
        static member inline genericNumber (x:string, _:bigint) = bigint.Parse x
        static member inline genericNumber (x:string, _:decimal) = decimal x
        static member inline genericNumber (x:string, _:Complex) = Complex(float x, 0.0)

    let inline instance (a: ^a, b: ^b, c: ^c) = ((^a or ^b or ^c) : (static member genericNumber: ^b * ^c -> ^c) (b, c))
    let inline genericNumber num = instance (GenericNumber, num, Unchecked.defaultof<'b>)

    let inline FromZero () = LanguagePrimitives.GenericZero
    let inline FromOne () = LanguagePrimitives.GenericOne
    let inline FromInt32 n = genericNumber n
    let inline FromInt64 n = genericNumber n
    let inline FromString n = genericNumber n


let filterMap<'a, 'b> (filterMapFunc: 'a -> 'b Option) (list: 'a List): 'b List =
    List.fold 
        (fun newList value -> 
            let newValue = filterMapFunc value
            match newValue with
            | Some a -> a :: newList
            | None -> newList) 
        []
        list

let flip f a b = f b a


//type A = { name: string; }

//type B = { name: string; zzz: float }
//type C = { name: string; zzz: float; xxx: double }
//let d = LanguagePrimitives.GenericOne

//let e = d + 1.5



//let rec inline abc (g: ^a) (h: ^a): bool =
//    let q = (^a : (member name : string) g) = (^a : (member name : string) h)
//    q = (abc h g)

//let r = abc {name = ""; zzz = 1.0} {name = ""; zzz = 1.0}