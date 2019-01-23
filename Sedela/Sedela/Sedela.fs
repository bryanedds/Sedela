namespace Sedela
open System
open System.Collections.Generic
open FParsec
open Prime
#nowarn "40"

[<ReferenceEquality>]
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

[<ReferenceEquality>]
type Block' =
    { ParentOpt : Block' option
      Children : Block' List
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
        let mutable depth = -1
        let mutable block = block
        while Option.isNone block.ParentOpt do depth <- inc depth
        depth

    static member getText (textFull : string) block =
        textFull.Substring (int block.PositionBegin.Index, int block.PositionEnd.Index)

    static member toIndex block =
        block.PositionBegin.Index

    static member fromIndex index block =
        let leaves = Block'.getLeaves block
        let candidates =
            Array.filter
                (fun leaf -> index >= leaf.PositionBegin.Index && index < leaf.PositionEnd.Index)
                leaves
        Array.head candidates

    static member getAncestors block =
        let ancestors = List ()
        let mutable block = block
        while Option.isSome block.ParentOpt do
            block <- block.ParentOpt.Value
            ancestors.Add block
        Array.ofSeq ancestors

    static member isAncestor a b =
        let ancestors = Block'.getAncestors b
        Array.contains a ancestors

    static member makeShallow (block : Block) (parentOpt : Block' option) =
        let block' =
            { ParentOpt = parentOpt
              Children = List ()
              PositionBegin = block.PositionBegin
              PositionEnd = block.PositionEnd }
        block'
        
    static member makeFromBlock block parentOpt =
        let rec import (block : Block) (block' : Block') =
            for child in block.Children do
                let child' = Block'.makeShallow child (Some block')
                block'.Children.Add child'
                import child block'
        let block' = Block'.makeShallow block parentOpt
        import block block'
        block'

    static member makeFromBlocks blocks positionBegin positionEnd =
        let root = { ParentOpt = None; Children = List (); PositionBegin = positionBegin; PositionEnd = positionEnd }
        for block in blocks do
            let block' = Block'.makeFromBlock block (Some root)
            root.Children.Add block'
        root

type BlockState =
    { Limiter : Block'
      Root : Block' }

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

type Expr =
    | Unit
    | Binding of string
    | If of Expr * Expr * Expr
    | Let of string * Expr
    | Derivation of Expr list

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

    let skipLineComment<'a> : Parser<unit, 'a> =
        parse {
            do! skipString LineCommentStr
            do! skipRestOfLine true }

    let skipWhitespace<'a> : Parser<unit, 'a> =
        skipLineComment <|>
        skipAnyOf WhitespaceChars

    let skipWhitespaces<'a> : Parser<unit, 'a> =
        skipMany skipWhitespace<'a>

    let recur parser =
        attempt
            (parse {
                do! eof
                return None } <|>
             parser)

    let backtrack (position : Position) =
        (fun stream ->
            stream.Seek position.Index
            Reply (ReplyStatus.Ok, NoErrorMessages)) >>.
        parse { return None }

    let parseLine =
        parse {
            let! offsetText = many (satisfy (fun char -> OffsetChars.IndexOf char > -1))
            let offset = List.length offsetText
            let! text = manyCharsTill anyChar (skipLineComment <|> skipNewline <|> eof)
            let textTrimmed = text.TrimEnd ()
            return (offset, textTrimmed) }

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
            let block' =
                let positionBegin = Position ("", 0L, 0L, 0L)
                let positionEnd = Position ("", int64 str.Length, 0L, 0L)
                Block'.makeFromBlocks blocks positionBegin positionEnd
            block'
        | Failure (error, _, _) -> failwith error

    let (parseExpr : Parser<Expr, BlockState>, private parseExprRef : Parser<Expr, BlockState> ref) =
        createParserForwardedToRef ()

    let parseAtom : Parser<string, BlockState> =
        parse {
            let! chars = many1 (noneOf (StructureChars + WhitespaceChars))
            let str = (chars |> String.implode).TrimEnd ()
            return str }

    let parseTill : Parser<Expr, BlockState> =
        parse {
            let! position = getPosition
            let! state = getUserState
            let currentBlock = Block'.fromIndex position.Index state.Root
            if Block'.isAncestor currentBlock state.Limiter
            then return! fail "End of derivation."
            else return Unit }

    let subparse parse : Parser<_, BlockState> =
        Primitives.parse {
            let! position = getPosition
            let! oldState = getUserState
            do! updateUserState (fun state -> { state with Limiter = Block'.fromIndex position.Index state.Root })
            let! expr = parse
            do! setUserState oldState
            return expr }

    let parseSubexprs =
        many1Till (subparse parseExpr) parseTill

    let parseBinding : Parser<Expr, BlockState> =
        parse {
            let! atomStr = parseAtom
            do! skipWhitespaces
            return Binding atomStr }

    let parseDerivationEnclosed : Parser<Expr, BlockState> =
        parse {
            do! skipString "("
            do! skipWhitespaces
            let! exprs = many1Till (subparse parseExpr) parseTill
            do! skipString ")"
            do! skipWhitespaces
            return Derivation exprs }

    let rec parseDerivation : Parser<Expr, BlockState> =
        parse {
            let! exprs =
                subparse
                    (Primitives.parse {
                        let! exprs =
                            many1Till
                                (parseDerivationEnclosed <|> parseDerivation)
                                parseTill
                        return exprs })
            return Derivation exprs }

    let parseLet : Parser<Expr, BlockState>=
        parse {

            let! letPosition = getPosition
            let! letState = getUserState
            let letBlock = Block'.fromIndex letPosition.Index letState.Root
            do! skipString "let"
            do! skipWhitespaces
            let! binding = parseAtom
            do! skipWhitespaces
            do! skipString "="
            do! skipWhitespaces
            let! body = subparse parseExpr
            return Let (binding, body) }

    let parseIf =
        parse {
            
            // if
            let! ifPosition = getPosition
            let! ifState = getUserState
            let ifBlock = Block'.fromIndex ifPosition.Index ifState.Root
            do! skipString "if"
            do! skipWhitespaces
            let! predicate = subparse parseExpr
            do! skipWhitespaces

            // then
            let! thenPosition = getPosition
            let! thenState = getUserState
            let thenBlock = Block'.fromIndex thenPosition.Index thenState.Root
            do! skipString "then"
            do! skipWhitespaces
            let! consequent = subparse parseExpr
            if thenBlock.ParentOpt = ifBlock.ParentOpt then
                
                // else
                let! elsePosition = getPosition
                let! elseState = getUserState
                let elseBlock = Block'.fromIndex elsePosition.Index elseState.Root
                do! skipString "else"
                do! skipWhitespaces
                let! alternative = subparse parseExpr
                if elseBlock.ParentOpt = ifBlock.ParentOpt
                then return If (predicate, consequent, alternative)
                else return! fail "Invalid if layout."
                
            // failure
            else return! fail "Invalid if layout." }
            
    do parseExprRef :=
        attempt parseBinding <|>
        attempt parseDerivation <|>
        attempt parseLet <|>
        attempt parseIf