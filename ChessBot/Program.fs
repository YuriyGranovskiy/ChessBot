module ChessBot.Program

open ChessBot.Entities.Squares
open ChessBot.Entities.Pieces
open ChessBot.Entities.Board
    

[<EntryPoint>]
let main args =
    let square = Square(File.A, Rank.R8)
    let bishop = Piece(PieceTypes.Bishop, PieceColors.White, square)
    
    let chessBoard = ChessBoard()
    chessBoard.ChessSquares[2,0].Piece <- Some bishop
    printfn $"%s{bishop.Describe()}"
    0
