// Learn more about F# at http://fsharp.org

open System.IO
open Sedela

[<EntryPoint>]
let main _ =
    let str = File.ReadAllText "Test.txt"
    let _ = Sedela.tryParseFromString str
    0 // return an integer exit code
