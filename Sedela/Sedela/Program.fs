// Learn more about F# at http://fsharp.org

open System.IO
open Sedela

[<EntryPoint>]
let main _ =
    let str = File.ReadAllText "Test.txt"
    let blocks = Sedela.parseBlocksFromString str
    ignore blocks
    0 // return an integer exit code
