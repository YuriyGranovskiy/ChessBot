module ChessBot.Entities.Game

open ChessBot.Entities.Board
open ChessBot.Entities.Pieces

type ChessGame(board: ChessBoard) =
    
    let mutable moveNumber = 1
    let mutable currentColor = PieceColors.White
    
    member this.pawnMove(move:string, color: PieceColors) =
        printfn $"move the {color} pawn to {move}" 
    member this.bishopMove(move:string, color: PieceColors) =
        printfn $"move the {color} bishop to {move}"
    member this.knightMove(move:string, color: PieceColors) =
        printfn $"move the {color} knight to {move}" 
    
    member this.Board = board
    member this.Move (move:string) =
        match move.[0] with
        | 'e' -> this.pawnMove(move, currentColor)
        | 'B' -> this.bishopMove(move.[1..], currentColor)
        | 'N' -> this.knightMove(move.[1..], currentColor)
        | _ -> raise (System.ArgumentException("Unsupported move"))
        
        if currentColor = Black then
            moveNumber <- moveNumber + 1
        if currentColor = White then currentColor <- Black
        else currentColor <- White