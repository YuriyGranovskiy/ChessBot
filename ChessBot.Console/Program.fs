module ChessBot.Program

open ChessBot.Entities.Board
open ChessBot.Entities.Game
    

[<EntryPoint>]
let main args =
    
    let chessBoard = ChessBoard.Default
    
    let game = ChessGame(chessBoard)
    
    game.Move("e4")
    game.Move("e5")
    chessBoard.Print()
    game.Move("Nf3")
    game.Move("Nc6")
    game.Move("Bc4")
    0