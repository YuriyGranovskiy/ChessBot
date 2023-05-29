module ChessBot.Entities.Game

open ChessBot.Entities.Board
open ChessBot.Entities.Pieces
open ChessBot.Exceptions.ValidationExceptions
open ChessBot.Helpers.Notation
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
            | Some p -> Some square
            | _ -> None

    let findPawnSquareToMove(color: PieceColors, fileTo: int, rankTo: int, chessSquares: ChessSquare[,]) : ChessSquare =
        let source = if color = PieceColors.White then List.rev [1 .. rankTo - 1] else [rankTo + 1 .. 6]
        let chessSquare : Option<ChessSquare> =
                                                Some (source |>
                                                    Seq.pick(fun r  ->
                                                        getSquareWithPiece(chessSquares.[fileTo,r])))                                                    
        match chessSquare with
        | Some cs -> if cs.Piece.Value.PieceColor = color && cs.Piece.Value.PieceType = Pawn then
                        cs
                     else
                         raise <| ImpossibleMove($"Not a valid piece. Expected {cs.Piece.Value.PieceColor} {cs.Piece.Value.PieceType}")                         
        | None -> raise <| ImpossibleMove("No pawn found")
    
    member this.pawnMove(move:string, color: PieceColors) =
        let file = GetFileIndexByChar move[0]
        let rank = GetRankIndexByChar move[1]
        moveThePiece(findPawnSquareToMove(color, file, rank, this.Board.ChessSquares), this.Board.ChessSquares.[file, rank])                
        
    member this.bishopMove(move:string, color: PieceColors) =
        printfn $"move the {color} bishop to {move}"
    member this.knightMove(move:string, color: PieceColors) =
        printfn $"move the {color} knight to {move}"
    
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