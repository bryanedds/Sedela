namespace Sedela
open System
open System.Collections.Generic
open FParsec
open Prime

type Offset = int

type Block =
    { Id : Guid
      Text : string
      Incr : int
      Next : Block option
      Appends : Block List }

type Block' =
    { Entries : (string * Block' option) list }

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

    let parseOffset =
        many (satisfy (fun char -> OffsetChars.IndexOf char > -1))

    let rec parseBlock offset incr =
        parse {
            let! offsetChars = parseOffset
            if List.length offsetChars = offset then
                let! text = restOfLine true
                let! block = attempt (parseBlock offset incr)
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Appends = List () }
            elif List.length offsetChars > offset then
                let offset = List.length offsetChars
                let! text = restOfLine true
                let incr = int incr
                let! block = attempt (parseBlock offset incr)
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Appends = List () }
            else
                do! fail "Reached block terminus."
                return None }

    let parseBlocks =
        many (parseBlock 0 0)

    let getBlockChildren block =
        let rec getBlockChildren' block (list : Block List) =
            match block.Next with
            | Some blockNext ->
                list.Add blockNext
                getBlockChildren' blockNext list
            | None -> ()
        let result = List ()
        getBlockChildren' block result
        List.ofSeq result

    let flattenBlock blockParent blockChildren =
        let mutable blockParent = blockParent
        for blockChild in blockChildren do
            if blockChild.Incr = blockParent.Incr then
                () // orphan - nothing to do
            elif blockChild.Incr = blockParent.Incr - 1 then
                blockParent.Appends.Add blockChild
            elif blockChild.Incr = blockParent.Incr - 2 then
                blockParent <- blockParent.Next.Value
                blockParent.Appends.Add blockChild
            else failwithumf ()

    let flattenBlocks blocks =
        List.iter
            (fun block -> flattenBlock block (getBlockChildren block))
            blocks

    let terminateBlocks blocks =
        match blocks with
        | head :: tail when head.Incr = 0 ->
            let mutable blockTop = head
            for block in tail do
                if block.Incr > 0 then
                    let blockChildren = getBlockChildren block
                    let blockEnclosing = List.findBack (fun block2 -> block2.Incr = block.Incr - 1) blockChildren
                    blockEnclosing.Appends.Add block
                    flattenBlock blockEnclosing blockChildren
                else blockTop <- block
        | _ -> failwith "No blocks found or first expression does not start on first column."

    let parseBlocksFromString str =
        match run parseBlocks str with
        | Success (blockOpts, _, _) ->
            let blocks = List.definitize blockOpts
            terminateBlocks blocks
            flattenBlocks blocks
            blocks
        | Failure (error, _, _) -> failwith error