module ChessBot.Entities.Game

open ChessBot.Entities.Board
open ChessBot.Entities.Pieces
open ChessBot.Helpers.Notation

type ChessGame(board: ChessBoard) =
    
    let mutable moveNumber = 1
    let mutable currentColor = PieceColors.White

    member this.pawnMove(move:string, color: PieceColors) =
        let file = GetFileIndexByChar(move[0])
        let rank = GetRankIndexByChar(move[1])
        if color = PieceColors.White then
            for i = rank - 2 downto 1 do
                let pawn = this.Board.ChessSquares.[file, i].Piece
                if pawn.IsSome then
                    this.Board.ChessSquares.[file, rank].Piece <- pawn
                    this.Board.ChessSquares.[file, i].Piece <- None
                
        else
            for i = rank to 6 do
                let pawn = this.Board.ChessSquares.[file, i].Piece
                if pawn.IsSome then
                    this.Board.ChessSquares.[file, rank].Piece <- pawn
                    this.Board.ChessSquares.[file, i].Piece <- None
        
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
        
        if currentColor = Black then
            moveNumber <- moveNumber + 1
        if currentColor = White then currentColor <- Black
        else currentColor <- White