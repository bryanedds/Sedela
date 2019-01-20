namespace Sedela
open System
open System.Collections.Generic
open FParsec
open Prime

type Block =
    { Text : string
      Children : Block List
      Next : Block option
      Incr : int
      PositionBegin : Position
      PositionEnd : Position
      Id : Guid }

    static member getChildren block =
        let rec getBlockChildren' block (list : Block List) =
            match block.Next with
            | Some blockNext ->
                list.Add blockNext
                getBlockChildren' blockNext list
            | None -> ()
        let result = List ()
        getBlockChildren' block result
        List.ofSeq result

    static member getFamily block =
        block :: Block.getChildren block

    static member flatten blockParent blockChildren =
        let mutable blockParent = blockParent
        for blockChild in blockChildren do
            if blockChild.Incr = blockParent.Incr + 1 then
                blockParent.Children.Add blockChild
            elif blockChild.Incr = blockParent.Incr + 2 then
                blockParent <- blockParent.Next.Value
                blockParent.Children.Add blockChild

    static member flattenMany blocks =
        List.map
            (fun block -> Block.flatten block (Block.getChildren block); block)
            blocks

    static member fixUp blocks =
        match blocks with
        | head :: tail when head.Incr = 0 ->
            let mutable blockOuterTop = head
            for blockInner in tail do
                if blockInner.Incr > 0 then
                    let blockOuterFamily = Block.getFamily blockOuterTop
                    let blockOuterParent = List.findBack (fun block2 -> block2.Incr = blockInner.Incr - 1) blockOuterFamily
                    let blockInnerFamily = Block.getFamily blockInner
                    Block.flatten blockOuterParent blockInnerFamily
                else blockOuterTop <- blockInner
            List.filter (fun block -> block.Incr = 0) blocks
        | _ -> failwith "No blocks found or first expression does not start on first column."

type Block' =
    { mutable ParentOpt : Block' option
      Children : Block' List
      Text : string
      PositionBegin : Position
      PositionEnd : Position }

    static member getFlattened block =
        let rec flatten' block (blocks : Block' List) =
            blocks.Add block
            for child in block.Children do flatten' child blocks
        let blocks = List ()
        flatten' block blocks
        Array.ofSeq blocks

    static member getLeaves block =
        let blocks = Block'.getFlattened block
        let leaves = Array.filter (fun block -> block.Children.Count = 0) blocks
        leaves

    static member getDepth block =
        let mutable depth = 0
        let mutable block = block
        while Option.isNone block.ParentOpt do depth <- inc depth
        depth

    static member toIndex block =
        block.PositionBegin.Index

    static member fromIndex index block =
        let leaves = Block'.getLeaves block
        let candidates =
            Array.filter
                (fun leaf -> index >= leaf.PositionBegin.Index && index < leaf.PositionEnd.Index)
                leaves
        let leafOpt = Seq.headOrDefault candidates
        leafOpt

    static member makeShallow (block : Block) =
        let block' =
            { ParentOpt = None
              Children = List ()
              Text = block.Text
              PositionBegin = block.PositionBegin
              PositionEnd = block.PositionEnd }
        block'
        
    static member make block =
        let rec import (block : Block) (block' : Block') =
            for child in block.Children do
                let child' = Block'.makeShallow child
                block'.Children.Add child'
                import child block'
        let block' = Block'.makeShallow block
        import block block'

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
            let! positionBegin = getPosition
            let! (offsetChars, text) = parseLine
            if  String.isEmpty text ||
                offsetChars = offset then
                let! block = recur (parseBlock offset incr)
                let! positionEnd = getPosition
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Children = List (); PositionBegin = positionBegin; PositionEnd = positionEnd }
            elif offsetChars > offset then
                let offset = offsetChars
                let incr = inc incr
                let! block = recur (parseBlock offset incr)
                let! positionEnd = getPosition
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = block; Children = List (); PositionBegin = positionBegin; PositionEnd = positionEnd }
            else return! backtrack positionBegin }

    let parseBlocks =
        manyTill (parseBlock 0 0) eof

    let parseBlocksFromString str =
        match run parseBlocks str with
        | Success (blockOpts, _, _) ->
            let blocks = List.definitize blockOpts
            let blocks = Block.flattenMany blocks
            let blocks = Block.fixUp blocks
            blocks
        | Failure (error, _, _) -> failwith error