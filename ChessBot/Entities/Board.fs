module ChessBot.Entities.Board

open ChessBot.Entities.Pieces
open ChessBot.Entities.Squares

type ChessSquare(square: Square) =
    let mutable piece : Option<Piece> = None
    member this.Square = square
    member this.Piece
        with get() = piece
        and set value = piece <- value

type ChessBoard() =
    let chessSquares : ChessSquare[,] = Array2D.zeroCreate 8 8
    
    do for i = 0 to 7 do
        for j = 0 to 7 do
            chessSquares.[i, j] <- ChessSquare(Square(LanguagePrimitives.EnumOfValue (j + 1), LanguagePrimitives.EnumOfValue (i + 1)))
            
    member this.ChessSquares = chessSquares