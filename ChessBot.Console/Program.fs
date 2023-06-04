module ChessBot.Program

open ChessBot.Console.Helpers
open ChessBot.Entities.Board
open ChessBot.Entities.Game
open ChessBot.Entities.Pieces    

[<EntryPoint>]
let main args =
    let chessBoard = ChessBoard.Default    
    let game = ChessGame(chessBoard)
    printfn "Make your move"
    while true do
        let move = game.MoveNumber
        if game.CurrentColor = White then printf $"\r\n{move}."
        game.Move(ConsoleHelper.readTheMove() )
        if game.CurrentColor = White then
            printfn ""
            chessBoard.Print()
    0