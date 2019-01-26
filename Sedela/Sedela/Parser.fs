﻿namespace Sedela
open System
open System.Collections.Generic
open FParsec
open Prime
#nowarn "21"
#nowarn "40"
module Parser =

    [<ReferenceEquality>]
    type Chunk =
        { Text : string
          Children : Chunk List
          Next : Chunk option
          Incr : int
          PositionBegin : Position
          PositionEnd : Position
          Id : Guid }

        static member getChildren chunk =
            let rec getChunkChildren' chunk (list : Chunk List) =
                match chunk.Next with
                | Some chunkNext ->
                    list.Add chunkNext
                    getChunkChildren' chunkNext list
                | None -> ()
            let result = List ()
            getChunkChildren' chunk result
            List.ofSeq result

        static member getFamily chunk =
            chunk :: Chunk.getChildren chunk

        static member flatten chunkParent chunkChildren =
            let mutable chunkParent = chunkParent
            for chunkChild in chunkChildren do
                if chunkChild.Incr = chunkParent.Incr + 1 then
                    chunkParent.Children.Add chunkChild
                elif chunkChild.Incr = chunkParent.Incr + 2 then
                    chunkParent <- chunkParent.Next.Value
                    chunkParent.Children.Add chunkChild

        static member flattenMany chunks =
            List.map
                (fun chunk -> Chunk.flatten chunk (Chunk.getChildren chunk); chunk)
                chunks

        static member fixUp chunks =
            match chunks with
            | head :: tail when head.Incr = 0 ->
                let mutable chunkOuterTop = head
                for chunkInner in tail do
                    if chunkInner.Incr > 0 then
                        let chunkOuterFamily = Chunk.getFamily chunkOuterTop
                        let chunkOuterParent = List.findBack (fun chunk2 -> chunk2.Incr = chunkInner.Incr - 1) chunkOuterFamily
                        let chunkInnerFamily = Chunk.getFamily chunkInner
                        Chunk.flatten chunkOuterParent chunkInnerFamily
                    else chunkOuterTop <- chunkInner
                List.filter (fun chunk -> chunk.Incr = 0) chunks
            | _ -> failwith "No chunks found or first expression does not start on first column."

    [<ReferenceEquality>]
    type Block =
        { Text : string
          ParentOpt : Block option
          Children : Block List
          PositionBegin : Position
          PositionEnd : Position }

        static member isRoot block =
            Option.isNone block.ParentOpt

        static member getRoot block =
            match block.ParentOpt with
            | Some parent -> Block.getRoot parent
            | None -> block

        static member getFlattened block =
            let rec flatten' block (blocks : Block List) =
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
            let blocks = Block.getFlattened block
            let candidates = Array.filter (Block.containsIndex index) blocks
            match Array.sortBy Block.getLength candidates with
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
            let ancestors = Block.getAncestors b
            Array.contains a ancestors

        static member makeShallow (chunk : Chunk) (parentOpt : Block option) =
            let block =
                { Text = chunk.Text
                  ParentOpt = parentOpt
                  Children = List ()
                  PositionBegin = chunk.PositionBegin
                  PositionEnd = chunk.PositionEnd }
            block
        
        static member makeFromChunk chunk parentOpt =
            let rec import (chunk : Chunk) (block : Block) =
                for child in chunk.Children do
                    let child' = Block.makeShallow child (Some block)
                    block.Children.Add child'
                    import child child'
            let block = Block.makeShallow chunk parentOpt
            import chunk block
            block

        static member makeFromChunks chunks positionBegin positionEnd =
            let root =
                { Text = ""
                  ParentOpt = None
                  Children = List ()
                  PositionBegin = positionBegin
                  PositionEnd = positionEnd }
            for chunk in chunks do
                let block = Block.makeFromChunk chunk (Some root)
                root.Children.Add block
            root

    type ParseScope =
        { Limiter : Block
          Root : Block }

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
        | Apply of Expr * Expr list

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] OffsetChars = "\t "
    let [<Literal>] WhitespaceChars = OffsetChars + NewlineChars
    let [<Literal>] LineCommentStr = "//"
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
        attempt ^ parse {
            do! eof
            return None } <|>
            parser

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

    let rec parseChunk offset incr =
        parse {
            let! positionBegin = getPosition
            let! (offsetChars, text) = parseLine
            if  String.isEmpty text ||
                offsetChars = offset then
                let! chunk = recur (parseChunk offset incr)
                let! positionEnd = getPosition
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = chunk; Children = List (); PositionBegin = positionBegin; PositionEnd = positionEnd }
            elif offsetChars > offset then
                let offset = offsetChars
                let incr = inc incr
                let! chunk = recur (parseChunk offset incr)
                let! positionEnd = getPosition
                return Some { Id = makeGuid (); Text = text; Incr = incr; Next = chunk; Children = List (); PositionBegin = positionBegin; PositionEnd = positionEnd }
            else return! backtrack positionBegin }

    let parseChunks =
        manyTill (parseChunk 0 0) eof

    let tryParseBlock str =
        match run parseChunks str with
        | Success (chunkOpts, _, _) ->
            let chunks = List.definitize chunkOpts
            let chunks = Chunk.flattenMany chunks
            let chunks = Chunk.fixUp chunks
            let block =
                let positionBegin = Position ("", 0L, 0L, 0L)
                let positionEnd = Position ("", int64 str.Length, 0L, 0L)
                Block.makeFromChunks chunks positionBegin positionEnd
            Right block
        | Failure (error, _, _) -> Left error

    let (parseExpr : Parser<Expr, ParseScope>, private parseExprRef : Parser<Expr, ParseScope> ref) =
        createParserForwardedToRef ()

    let parseExprs =
        many parseExpr

    let pushScope =
        parse {
            let! position = getPosition
            let! oldScope = getUserState
            let currentScope = { oldScope with Limiter = Block.fromIndex position.Index oldScope.Root }
            do! setUserState currentScope
            return oldScope }

    let popScope oldScope =
        parse {
            do! setUserState oldScope }

    let inScope parse =
        Primitives.parse {
            let! position = getPosition
            let! currentScope = getUserState
            let currentBlock = Block.fromIndex position.Index currentScope.Root
            if currentBlock = currentScope.Limiter || Block.isAncestor currentScope.Limiter currentBlock
            then return! parse
            else return! fail "End of block." }

    let parseAtom =
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

    let parseBinding =
        parse {
            do! notFollowedBy skipForm
            let! atomStr = parseAtom
            do! skipWhitespaces
            return Binding atomStr }

    let parseUnit =
        parse {
            do! skipString "()"
            do! skipWhitespaces
            return Unit }

    let parseEnclosure =
        parse {
            let! openingScope = pushScope
            do! skipString "("
            do! skipWhitespaces
            let! expr = inScope parseExpr
            do! inScope (skipString ")")
            do! skipWhitespaces
            do! popScope openingScope
            return expr }

    let parseApplyFragment =
        attempt parseEnclosure <|>
        attempt parseBinding <|>
        attempt parseUnit

    let rec parseApply =
        parse {
            let! meaningScope = pushScope
            let! meaning = parseApplyFragment
            let! args = many1 ^ parse {
                let! position = getPosition
                let! currentScope = getUserState
                let currentBlock = Block.fromIndex position.Index currentScope.Root
                return!
                    if currentBlock.ParentOpt = Some currentScope.Limiter
                    then inScope (attempt parseApply <|> parseApplyFragment)
                    else inScope parseApplyFragment }
            do! popScope meaningScope
            return Apply (meaning, args) }

    let parseLet =
        parse {

            // binding
            let! letScope = pushScope
            do! skipLet
            do! skipWhitespaces
            let! binding = inScope parseAtom

            // body
            do! skipWhitespaces
            do! inScope (skipAtom "=")
            do! skipWhitespaces
            let! body = inScope parseExpr

            // fin
            do! popScope letScope
            return Let (binding, body) }

    let parseIf =
        parse {

            // if
            do! skipIf
            do! skipWhitespaces
            let! predicate = inScope parseExpr

            // then
            do! skipThen
            do! skipWhitespaces
            let! consequent = inScope parseExpr

            // else
            do! skipElse
            do! skipWhitespaces
            let! alternative = inScope parseExpr

            // fin
            return If (predicate, consequent, alternative) }

    do parseExprRef :=
        attempt parseLet <|>
        attempt parseIf <|>
        attempt parseApply <|>
        attempt parseEnclosure <|>
        attempt parseBinding <|>
        attempt parseUnit

    let tryParseFromString str =
        match tryParseBlock str with
        | Right root ->
            match runParserOnString parseExprs { Root = root; Limiter = root } "" str with
            | Success (exprs, _, _) -> Right exprs
            | Failure (error, _, _) -> Left error
        | Left error -> Left error