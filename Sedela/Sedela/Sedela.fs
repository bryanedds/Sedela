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
    { Text : string
      ParentOpt : Block' option
      Children : Block' List
      PositionBegin : Position
      PositionEnd : Position }

    static member isRoot block =
        Option.isNone block.ParentOpt

    static member getRoot block =
        match block.ParentOpt with
        | Some parent -> Block'.getRoot parent
        | None -> block

    static member getFlattened block =
        let rec flatten' block (blocks : Block' List) =
            blocks.Add block
            for child in block.Children do flatten' child blocks
        let blocks = List ()
        flatten' block blocks
        Array.ofSeq blocks

    static member getDepth block =
        let mutable depth = -1
        let mutable block = block
        while Option.isNone block.ParentOpt do depth <- inc depth
        depth

    static member getLength block =
        block.PositionEnd.Index - block.PositionBegin.Index

    static member getText (fullText : string) block =
        fullText.Substring (int block.PositionBegin.Index, int block.PositionEnd.Index)

    static member containsIndex index block =
        index >= block.PositionBegin.Index && index < block.PositionEnd.Index

    static member toIndex block =
        block.PositionBegin.Index

    static member fromIndex index block =
        let blocks = Block'.getFlattened block
        let candidates = Array.filter (Block'.containsIndex index) blocks
        match Array.sortBy Block'.getLength candidates with
        | [||] -> block
        | ordered -> Array.head ordered

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
            { Text = block.Text
              ParentOpt = parentOpt
              Children = List ()
              PositionBegin = block.PositionBegin
              PositionEnd = block.PositionEnd }
        block'
        
    static member makeFromBlock block parentOpt =
        let rec import (block : Block) (block' : Block') =
            for child in block.Children do
                let child' = Block'.makeShallow child (Some block')
                block'.Children.Add child'
                import child child'
        let block' = Block'.makeShallow block parentOpt
        import block block'
        block'

    static member makeFromBlocks blocks positionBegin positionEnd =
        let root =
            { Text = ""
              ParentOpt = None
              Children = List ()
              PositionBegin = positionBegin
              PositionEnd = positionEnd }
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
    | Derivation of Expr * Expr list

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

    let skip1Whitespaces<'a> : Parser<unit, 'a> =
        skipMany1 skipWhitespace<'a>

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

    let tryParseBlockFromString str =
        match run parseBlocks str with
        | Success (blockOpts, _, _) ->
            let blocks = List.definitize blockOpts
            let blocks = Block.flattenMany blocks
            let blocks = Block.fixUp blocks
            let block' =
                let positionBegin = Position ("", 0L, 0L, 0L)
                let positionEnd = Position ("", int64 str.Length, 0L, 0L)
                Block'.makeFromBlocks blocks positionBegin positionEnd
            Right block'
        | Failure (error, _, _) -> Left error

    let (parseExpr : Parser<Expr, BlockState>, private parseExprRef : Parser<Expr, BlockState> ref) =
        createParserForwardedToRef ()

    let parseExprs =
        many parseExpr

    let pushState =
        parse {
            let! position = getPosition
            let! oldState = getUserState
            let currentBlock = Block'.fromIndex position.Index oldState.Root
            let currentState = { oldState with Limiter = currentBlock }
            do! setUserState currentState
            return oldState }

    let popState oldState =
        parse {
            do! setUserState oldState }

    let rec subState parse parse2 : Parser<_, BlockState> =
        Primitives.parse {
            let! oldState = pushState
            let! _ = withState parse
            let! expr = parse2
            do! popState oldState
            return expr }

    and withState parse : Parser<_, BlockState> =
        Primitives.parse {
            let! position = getPosition
            let! state = getUserState
            let currentBlock = Block'.fromIndex position.Index state.Root
            if currentBlock = state.Limiter || Block'.isAncestor state.Limiter currentBlock
            then return! parse
            else return! fail "End of block." }

    let parseAtom : Parser<string, BlockState> =
        parse {
            let! chars = many1 (noneOf (StructureChars + WhitespaceChars))
            let str = (chars |> String.implode).TrimEnd ()
            return str }

    let skipAtom str =
        parse {
            let! atom = parseAtom
            if atom = str
            then return ()
            else return! fail "Unexpected atom." }

    let skipLet = skipAtom "let"
    let skipIf = skipAtom "if"
    let skipThen = skipAtom "then"
    let skipElse = skipAtom "else"

    let skipForm =
        attempt skipLet <|>
        attempt skipIf <|>
        attempt skipThen <|>
        attempt skipElse

    let parseBinding : Parser<Expr, BlockState> =
        parse {
            do! notFollowedBy skipForm
            let! atomStr = parseAtom
            do! skipWhitespaces
            return Binding atomStr }

    let parseEnclosure : Parser<Expr, BlockState> =
        parse {
            let! oldState = pushState
            do! skipString "("
            do! skipWhitespaces
            let! expr = withState parseExpr
            do! withState (skipString ")")
            do! skipWhitespaces
            do! popState oldState
            return expr }

    let rec parseDerivation : Parser<Expr, BlockState> =
        parse {
            let! oldState = pushState
            let! meaning = attempt parseBinding <|> attempt parseEnclosure
            let! args = many1 (withState (attempt parseBinding <|> attempt parseEnclosure))
            do! popState oldState
            return Derivation (meaning, args) }

    let parseLet : Parser<Expr, BlockState> =
        parse {
            let! letState = pushState
            do! skipLet
            do! skipWhitespaces
            let! binding = withState parseAtom
            do! skipWhitespaces
            do! withState (skipAtom "=")
            do! skipWhitespaces
            let! body = withState parseExpr
            do! popState letState
            return Let (binding, body) }

    let parseIf =
        parse {
            
            // if
            let! ifState = pushState
            do! skipIf
            do! skipWhitespaces
            let! predicate = withState parseExpr

            // then
            let! thenState = withState pushState
            do! skipThen
            do! skipWhitespaces
            let! consequent = withState parseExpr
            do! popState thenState
                
            // else
            let! elseState = withState pushState
            do! skipElse
            do! skipWhitespaces
            let! alternative = withState parseExpr
            do! popState elseState

            // fin
            do! popState ifState
            return If (predicate, consequent, alternative) }
            
    do parseExprRef :=
        attempt parseLet <|>
        attempt parseIf <|>
        attempt parseEnclosure <|>
        attempt parseDerivation <|>
        attempt parseBinding

    let tryParseFromString str =
        match tryParseBlockFromString str with
        | Right block ->
            match runParserOnString parseExprs { Root = block; Limiter = block } "" str with
            | Success (exprs, _, _) -> Right exprs
            | Failure (error, _, _) -> Left error
        | Left error -> Left error