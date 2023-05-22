module ChessBot.Entities.Game

open ChessBot.Entities.Board
open ChessBot.Entities.Pieces

type ChessGame(board: ChessBoard) =
    
    let mutable moveNumber = 1
    let mutable currentColor = PieceColors.White

    let getRankIndexByFileChar(fileChar:char) : int =        
        let ascii = fileChar |> int 
        if ascii >= 97 && ascii <=104 then
            ascii - 97
        else if ascii >= 65 && ascii <=72 then
            ascii - 65
        else
            raise ( System.ArgumentException($"Improper code {fileChar}"))   

    member this.pawnMove(move:string, color: PieceColors) =
        let file = getRankIndexByFileChar(move[0])
        let rank = (move[1] |> int) - 49
        if color = PieceColors.White then
            for i = rank - 2 downto 1 do
                let pawn = this.Board.ChessSquares.[i, file].Piece
                if pawn.IsSome then
                    this.Board.ChessSquares.[rank, file].Piece <- pawn
                    this.Board.ChessSquares.[i, file].Piece <- None
                
        else
            for i = rank to 6 do
                let pawn = this.Board.ChessSquares.[i, rank].Piece
                if pawn.IsSome then
                    this.Board.ChessSquares.[rank, file].Piece <- pawn
                    this.Board.ChessSquares.[i, rank].Piece <- None
        
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