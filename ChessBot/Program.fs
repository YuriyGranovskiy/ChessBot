module ChessBot.Program

open ChessBot.Entities.Board
    

[<EntryPoint>]
let main args =
    
    let chessBoard = ChessBoard.Default
    
    let description1 = chessBoard.DescribeSquare("d7")
    let description2 = chessBoard.DescribeSquare("d6")
    printfn $"%s{description1}"
    printfn $"%s{description2}"
    0
