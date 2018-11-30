namespace Sedela
open System
open System.Collections.Generic
open FParsec
open Prime

type Offset = int

type Opening =
    | If // allows for \nthen, \nelif, \nelse, and indent connectives
    | Let // allows for indent connectuves
    | Segmented // allows for \n| connectives
    | Unparsed of string

type Node =
    Offset * Opening * string

type Connective =
    | NewlineIndent
    | NewlineThen
    | NewlineElif
    | NewlineElse
    | NewlineBar

(*type Structure =
    | Lead of Node * Structure list
    | Cont of Connective * Structure

type LineParse =
    | Complete of Structure
    | Overflow of Structure * int
    | Underflow of (Opening * string) list
    | Offsides of string*)

type LineParse =
    { Offset : int
      Text : string }

type Structure =
    | Structure of LineParse
    | Structures of Structure list

module Sedela =

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] OffsetChars = "\t "
    let [<Literal>] WhitespaceChars = OffsetChars + NewlineChars
    let [<Literal>] LineCommentStr = "//"

    let [<Literal>] OpenMultilineCommentStr = "(*"
    let [<Literal>] CloseMultilineCommentStr = "*)"

    let [<Literal>] ReservedChars = "{}\\#$"
    let [<Literal>] StructureCharsNoStr = "()"
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let (*Literal*) IllegalNameChars = ReservedChars + StructureChars + WhitespaceChars
    let (*Literal*) IllegalNameCharsArray = Array.ofSeq IllegalNameChars

    (*let skipNewline =
        skipSatisfy (fun char -> NewlineChars.IndexOf char > -1)

    let parseOffset =
        many (satisfy (fun char -> OffsetChars.IndexOf char > -1))

    let parseText =
        charsTillString NewlineChars false 0

    let parseLine =
        parse {
            let! offsetChars = parseOffset
            let! textChars = parseText
            return { Offset = List.length offsetChars; Text = textChars } }

    let parseLines =
        many (skipNewline >>. parseLine)

    let getBlock offset (lines : LineParse seq) =
        let block = Dictionary ()
        let (guid, guid2) = (makeGuid (), makeGuid ())
        let stack = Stack [(guid, guid2, offset)]
        let mutable going = true
        let mutable enr = lines.GetEnumerator ()
        while going && enr.MoveNext () do
            let line = enr.Current
            let (guid, guid2, offset) = stack.Peek ()
            if line.Offset = offset then
                block.Add (guid, guid2, line)
            elif line.Offset > offset then
                let (guid, guid2) = (makeGuid (), makeGuid ())
                stack.Push (guid, guid2, line.Offset)
                block.Add (guid, line)
            else
                stack.Pop () |> ignore
        (guid, guid2, block)

    let rec linesToStructure3 offset (lines : LineParse seq) results =

        let block = List ()
        let mutable offset = offset
        let mutable going = true
        let mutable enr = lines.GetEnumerator ()
        while going && enr.MoveNext () do
            let line = enr.Current
            if line.Offset >= offset then
                offset <- line.Offset
                block.Add line
            else going <- false

        let (candidates, lines) = List.split (fun line -> line.Offset >= offset) lines
        let _ =
            List.fold
                (fun structures line ->
                    linesToStructure3 offset)

                candidates
        (Structures left, right)

        match linesToStructureDown offset lines results with
        | (results, []) ->
        | (results, line :: lines) ->
            if lines.Offset = offset then
                (line :: 
        let result = _
        (result :: results, [])

    let linesToStructure lines =
        let rec linesToStructure lines offset =
            match lines with
            | line :: lines ->
                if line.Offset = offset then
                    Choice1Of3 (Structure line, lines)
                elif line.Offset > offset then
                    match linesToStructure lines line.Offset with
                    | Choice1Of3 (structure, lines) ->
                        
                        Right (Structures [Structure line; structure]) // down
                    | Choice2Of3 lines -> Left lines // forward
                    | Choice3Of3 _ -> // back
                else Choice3Of3 lines
                    
            | [] -> Structures []
        linesToStructure lines 0

    let parseStructure =
        parse {
            let! lines = parseLines
            return linesToStructure lines }

    let parser =
        parse {
            let! (nodes : Node list) = getUserState
            let! line = parseLine
            match line with
            | Complete structure ->
                return structure
            | Overflow (structure, finished) ->
                let nodes = List.skip finished nodes
                do! setUserState nodes
                return structure
            | Underflow nodes ->
                let underflowAmount = List.length nodes
                let! result = tryParseLines underflowAmount
                match result with
                | Right (continuations : (Connective * string) list) ->
                    // todo: combine nodes and continuations
                    return obj () :?> Structure
                | Left offsidesStr -> return obj () :?> Structure
            | Offsides str ->
                return obj () :?> Structure }*)