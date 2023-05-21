module ChessBot.Program

open ChessBot.Entities.Board
    

[<EntryPoint>]
let main args =
    
    let chessBoard = ChessBoard.Default
    
    let description = chessBoard.DescribeSquare("d7")
    printfn $"%s{description}"
    chessBoard.Print()
    0
