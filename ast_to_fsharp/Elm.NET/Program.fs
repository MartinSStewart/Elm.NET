// Learn more about F# at http://fsharp.org

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations



[<EntryPoint>]
let main argv =
    let a = Import.import
    //let rec 
    //    inline a z = b
    //    and 
    //    b = 
    //        let rec v = 6

    //        5
    //    and 
    //    c = 6
    //    and
    //    d = a 0 

    //let a : Expr = 
    //    <@@
    //        let b =
    //            0
    //        b
    //    @@>
    //let ifThen condition ifTrue ifFalse = 
    //    <@@
    //    if %%condition then
    //        %%ifTrue
    //    else
    //        %%ifFalse
    //    @@>

    
    //let b = 
    //    FSharpExpr { ImmediateSubExpressions = []; Range = 0 * 0; Type = FSharpType }//CombTerm
    0
