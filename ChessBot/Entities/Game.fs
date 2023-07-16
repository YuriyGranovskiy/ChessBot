module ChessBot.Entities.Game

open ChessBot.DTO.Dto
open ChessBot.Entities.Board
open ChessBot.Exceptions.ValidationExceptions
open ChessBot.Helpers.Notation
open ChessBot.Utils.SequenceUtils
open Microsoft.FSharp.Core

type ChessGame(board: ChessBoard) =

    let mutable moveNumber = 1
    let mutable currentColor = PieceColors.White

    let moveThePiece(squareFrom : ChessSquare, squareTo: ChessSquare) =
        let piece = squareFrom.Piece
        squareTo.Piece <- piece
        squareFrom.Piece <- None

    let getSquareWithPiece(square: ChessSquare) : Option<ChessSquare> =
        match square.Piece with
            | Some _ -> Some square
            | _ -> None

    let getSquareWithKnight(square: ChessSquare, color: PieceColors) : Option<ChessSquare> =
        match square.Piece with
            | Some p -> if p.PieceType = Knight && p.PieceColor = color then Some square else None
            | _ -> None

    let getValidPawnRanks(color: PieceColors, rankTo: int) =
        match color with
        | White -> if rankTo = 3 then List.rev [ 1 .. 2] else List.rev [ rankTo - 1 .. rankTo - 1]
        | Black -> if rankTo = 4 then [ 5 .. 6] else [ rankTo + 1 .. rankTo + 1]

    let getValidKnightSquaresByRanks(fileTo: int, rankTo:int, ranks: seq<int>) : seq<int*int> =
        ranks |> Seq.map(fun rank ->
            let fileDifference = 3 - abs (rankTo - rank)
            let fileRange: list<int> = [fileTo - fileDifference; fileTo + fileDifference]
            let validFileRange = fileRange |> Seq.where(fun f -> f >= 0 && f <= 7)
            let pairs: seq<int*int> = validFileRange |> Seq.map(fun f -> (f, rank))
            pairs) |> Seq.collect id

    let getValidKnightSources(fileTo: int, rankTo: int): seq<int * int> =
        let rankRange = [rankTo - 2 .. rankTo + 2] |> Seq.where( fun r -> r >= 0 && r<= 7 && r <> rankTo)
        getValidKnightSquaresByRanks(fileTo, rankTo, rankRange)

    let findKnightSquareToMove(color: PieceColors, fileTo: int, rankTo: int, chessSquares: ChessSquare[,]) : ChessSquare =
        let validSquares = getValidKnightSources(fileTo, rankTo)
        let squaresWithKnights = validSquares |> Seq.map(fun s -> getSquareWithKnight(chessSquares.[fst s, snd s], color)) |> onlySome        
        if (Seq.length squaresWithKnights) <> 1 then raise (ImpossibleMove("Can't find proper knight"))
        squaresWithKnights |> Seq.exactlyOne

    let findBishopSquareToMove(color: PieceColors, fileTo: int, rankTo: int, chessSquares: ChessSquare[,]) : ChessSquare =
        let downLeftSource = [1..min (7 - fileTo) (7 - rankTo)]
        let upLeftSource = [1..min (7-fileTo) rankTo]
        let downRightSource = [1..min fileTo (7 - rankTo)]
        let upRightSource = [1..min fileTo rankTo]
        let downLeftPossibleSquares:seq<int * int> = downLeftSource |> Seq.map(fun s -> (fileTo + s, rankTo + s))
        let upLeftPossibleSquares:seq<int * int> = upLeftSource |> Seq.map(fun s -> (fileTo + s, rankTo - s))
        let downRightPossibleSquares:seq<int * int> = downRightSource |> Seq.map(fun s -> (fileTo - s, rankTo + s))
        let upRightPossibleSquares:seq<int * int> = upRightSource |> Seq.map(fun s -> (fileTo - s, rankTo - s))
        let downLeftSquare: Option<ChessSquare> = downLeftPossibleSquares |> Seq.tryPick(fun s -> getSquareWithPiece(chessSquares.[fst s, snd s]))
        let upLeftSquare: Option<ChessSquare> = upLeftPossibleSquares |> Seq.tryPick(fun s -> getSquareWithPiece(chessSquares.[fst s, snd s]))
        let downRightSquare: Option<ChessSquare> = downRightPossibleSquares |> Seq.tryPick(fun s -> getSquareWithPiece(chessSquares.[fst s, snd s]))
        let upRightSquare: Option<ChessSquare> = upRightPossibleSquares |> Seq.tryPick(fun s -> getSquareWithPiece(chessSquares.[fst s, snd s]))
        let squares = [downLeftSquare; upLeftSquare; downRightSquare; upRightSquare] |> onlySome
        let bishopSquares = squares |> Seq.where(fun s -> s.Piece.Value.PieceColor = color && s.Piece.Value.PieceType = Bishop)
        if (Seq.length bishopSquares) <> 1 then raise (ImpossibleMove("Can't find proper bishop"))
        bishopSquares |> Seq.exactlyOne

    let findPawnSquareToMove(color: PieceColors, fileTo: int, rankTo: int, chessSquares: ChessSquare[,]) : ChessSquare =
        let source = getValidPawnRanks(color, rankTo)
        let sourceChessSquare : Option<ChessSquare> =
                                                source |>
                                                    Seq.tryPick(fun r  ->
                                                        getSquareWithPiece(chessSquares.[fileTo,r]))
        match sourceChessSquare with
        | Some cs -> if cs.Piece.Value.PieceColor = color && cs.Piece.Value.PieceType = Pawn then
                        cs
                     else
                         raise <| ImpossibleMove($"Not a valid piece. Expected {cs.Piece.Value.PieceColor} {cs.Piece.Value.PieceType}")                         
        | None -> raise <| ImpossibleMove("No pawn found")

    member this.MoveNumber
        with get () = moveNumber

    member this.CurrentColor
        with get() = currentColor

    member this.ToDto() : GameDto = 
        {Move = this.MoveNumber ; CurrentColorMove = this.CurrentColor; PiecePositions = Array.empty<PiecePositionDto>}

    member this.pawnMove(move:string, color: PieceColors) =
        let file = GetFileIndexByChar move[0]
        let rank = GetRankIndexByChar move[1]
        moveThePiece(findPawnSquareToMove(color, file, rank, this.Board.ChessSquares), this.Board.ChessSquares.[file, rank])                

    member this.bishopMove(move:string, color: PieceColors) =
        let file = GetFileIndexByChar move[0]
        let rank = GetRankIndexByChar move[1]
        moveThePiece(findBishopSquareToMove(color, file, rank, this.Board.ChessSquares), this.Board.ChessSquares.[file, rank])

    member this.knightMove(move:string, color: PieceColors) =
        let file = GetFileIndexByChar move[0]
        let rank = GetRankIndexByChar move[1]
        moveThePiece(findKnightSquareToMove(color, file, rank, this.Board.ChessSquares), this.Board.ChessSquares.[file, rank])

    member this.Board : ChessBoard = board
    member this.Move (move:string) =
        match move.[0] with
        | 'B' -> this.bishopMove(move.[1..], currentColor)
        | 'N' -> this.knightMove(move.[1..], currentColor)
        | _ -> this.pawnMove(move, currentColor)

        if currentColor = PieceColors.Black then
            moveNumber <- moveNumber + 1
        if currentColor = PieceColors.White then currentColor <- PieceColors.Black
        else currentColor <- PieceColors.White
