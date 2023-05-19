module ChessBot.Program

open ChessBot.Entities.Squares
open ChessBot.Entities.Pieces
    

[<EntryPoint>]
let main args =
    let square = Square(File.A, Rank.R8)
    let bishop = Piece(PieceTypes.Bishop, PieceColors.White, square)
    printfn $"%s{bishop.Describe()}"
    0
