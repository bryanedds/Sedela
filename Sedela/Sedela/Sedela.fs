namespace Sedela
open System
open System.Collections.Generic
open FParsec
open FParsec.CharParsers
open Prime

type Offset = int

type Block =
    { Text : string
      Children : Block List
      Next : Block option
      Incr : int
      Id : Guid }

type Block' =
    { Entries : (string * Block' option) list }

type Opening =
    | If // allows for \nthen, \nelif, \nelse, and indent connectives
    | Let // allows for indent connectuves
    | Segmented // allows for \n| connectives
    | Unparsed of string

type Connective =
    | NewlineIndent
    | NewlineThen
    | NewlineElif
    | NewlineElse
    | NewlineBar

module Sedela =
    open FParsec

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

    let parseComment =
        parse {
            do! skipString LineCommentStr
            do! skipRestOfLine true }

    let parseLine =
        parse {
            let! offsetText = many (satisfy (fun char -> OffsetChars.IndexOf char > -1))
            let offset = List.length offsetText
            let! text = manyCharsTill anyChar (parseComment <|> skipNewline <|> eof)
            let textTrimmed = text.TrimEnd ()
            return (offset, textTrimmed) }

    let recur parser =
        attempt
            (parse {
                do! eof
                return None } <|>
             parser)

    let backtrack (position : Position) : Parser<_, _> =
        (fun stream ->
            stream.Seek position.Index
            Reply (ReplyStatus.Ok, NoErrorMessages)) >>.
        parse { return None }

    let rec parseBlock offset incr =
        parse {
            let! position = getPosition
            let! (offsetChars, text) = parseLine
            if  String.isEmpty text ||
                offsetChars = offset then
                let! block = recur (parseBlock offset incr)
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Children = List () }
            elif offsetChars > offset then
                let offset = offsetChars
                let incr = inc incr
                let! block = recur (parseBlock offset incr)
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Children = List () }
            else return! backtrack position }

    let parseBlocks =
        manyTill (parseBlock 0 0) eof

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

    let getBlockFamily block =
        block :: getBlockChildren block

    let flattenBlock blockParent blockChildren =
        let mutable blockParent = blockParent
        for blockChild in blockChildren do
            if blockChild.Incr = blockParent.Incr + 1 then
                blockParent.Children.Add blockChild
            elif blockChild.Incr = blockParent.Incr + 2 then
                blockParent <- blockParent.Next.Value
                blockParent.Children.Add blockChild

    let flattenBlocks blocks =
        List.map
            (fun block -> flattenBlock block (getBlockChildren block); block)
            blocks

    let fixUpBlocks blocks =
        match blocks with
        | head :: tail when head.Incr = 0 ->
            let mutable blockOuterTop = head
            for blockInner in tail do
                if blockInner.Incr > 0 then
                    let blockOuterFamily = getBlockFamily blockOuterTop
                    let blockOuterParent = List.findBack (fun block2 -> block2.Incr = blockInner.Incr - 1) blockOuterFamily
                    let blockInnerFamily = getBlockFamily blockInner
                    flattenBlock blockOuterParent blockInnerFamily
                else blockOuterTop <- blockInner
            List.filter (fun block -> block.Incr = 0) blocks
        | _ -> failwith "No blocks found or first expression does not start on first column."

    let parseBlocksFromString str =
        match run parseBlocks str with
        | Success (blockOpts, _, _) ->
            let blocks = List.definitize blockOpts
            let blocks = flattenBlocks blocks
            let blocks = fixUpBlocks blocks
            blocks
        | Failure (error, _, _) -> failwith error