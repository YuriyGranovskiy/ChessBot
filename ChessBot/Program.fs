module ChessBot.Program

open ChessBot.Entities.Board
    

[<EntryPoint>]
let main args =
    let square = Square(File.A, Rank.R8)
    let color = square.Color
    printfn $"Square color is %s{color.ToString()}"
    0
