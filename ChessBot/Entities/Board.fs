module ChessBot.Entities.Board

open ChessBot.Entities.Pieces
open ChessBot.Entities.Squares
open ChessBot.Helpers.Notation

type ChessSquare(square: Square) =
    let mutable piece : Option<Piece> = None
    member this.Square = square
    member this.Piece
        with get() = piece
        and set value = piece <- value

type ChessBoard() =
    let chessSquares : ChessSquare[,] = Array2D.zeroCreate 8 8
    
    do for r = 0 to 7 do
        for f = 0 to 7 do
            chessSquares.[f, r] <- ChessSquare(Square(LanguagePrimitives.EnumOfValue (f + 1), LanguagePrimitives.EnumOfValue (r + 1)))

    let getLetterByPieceType(pieceType: PieceTypes, pieceColor: PieceColors) : string =
        if pieceColor = PieceColors.White then
            match pieceType with
            | PieceTypes.Pawn -> "P"
            | PieceTypes.Knight -> "N"
            | PieceTypes.Bishop -> "B"
            | PieceTypes.Rook -> "R"
            | PieceTypes.Queen -> "Q"
            | PieceTypes.King -> "K"
        else
            match pieceType with
            | PieceTypes.Pawn -> "p"
            | PieceTypes.Knight -> "n"
            | PieceTypes.Bishop -> "b"
            | PieceTypes.Rook -> "r"
            | PieceTypes.Queen -> "q"
            | PieceTypes.King -> "k"

    member this.ChessSquares = chessSquares    
    member this.getChessSquareByCode(code: string):ChessSquare =
        this.ChessSquares.[GetFileIndexByChar(code[0]), GetRankIndexByChar(code[1])]

    member this.DescribeSquare(code: string): string =
        let squareToDescribe = this.getChessSquareByCode(code)
        let color = $"%s{squareToDescribe.Square.Color.ToString()}"        
        match squareToDescribe.Piece with
        | Some p -> $"%s{p.Describe()} standing on a {color} square" 
        | None -> $"There is nothing on this {color} square"

    member this.Init(code:string, piece:Option<Piece>) =
        this.getChessSquareByCode(code).Piece <- piece

    member this.Print() =
        printfn " --- --- --- --- --- --- --- ---"
        for r = 0 to 7 do
            printf "|"
            for f = 0 to 7 do
                let piece = this.ChessSquares.[f, 7-r].Piece
                let result = match piece with
                                | Some p -> $" {getLetterByPieceType(p.PieceType, p.PieceColor)} "
                                | None -> "   "
                printf $"{result}|"
            printfn ""
            printfn " --- --- --- --- --- --- --- ---"
                
with static member Default =
        let defaultBoard = ChessBoard()
        defaultBoard.Init("a1", Some (Piece(PieceTypes.Rook, PieceColors.White)))
        defaultBoard.Init("b1", Some (Piece(PieceTypes.Knight, PieceColors.White)))
        defaultBoard.Init("c1", Some (Piece(PieceTypes.Bishop, PieceColors.White)))
        defaultBoard.Init("d1", Some (Piece(PieceTypes.Queen, PieceColors.White)))
        defaultBoard.Init("e1", Some (Piece(PieceTypes.King, PieceColors.White)))
        defaultBoard.Init("f1", Some (Piece(PieceTypes.Bishop, PieceColors.White)))
        defaultBoard.Init("g1", Some (Piece(PieceTypes.Knight, PieceColors.White)))
        defaultBoard.Init("h1", Some (Piece(PieceTypes.Rook, PieceColors.White)))
        
        defaultBoard.Init("a2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("b2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("c2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("d2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("e2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("f2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("g2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        defaultBoard.Init("h2", Some (Piece(PieceTypes.Pawn, PieceColors.White)))
        
        defaultBoard.Init("a8", Some (Piece(PieceTypes.Rook, PieceColors.Black)))
        defaultBoard.Init("b8", Some (Piece(PieceTypes.Knight, PieceColors.Black)))
        defaultBoard.Init("c8", Some (Piece(PieceTypes.Bishop, PieceColors.Black)))
        defaultBoard.Init("d8", Some (Piece(PieceTypes.Queen, PieceColors.Black)))
        defaultBoard.Init("e8", Some (Piece(PieceTypes.King, PieceColors.Black)))
        defaultBoard.Init("f8", Some (Piece(PieceTypes.Bishop, PieceColors.Black)))
        defaultBoard.Init("g8", Some (Piece(PieceTypes.Knight, PieceColors.Black)))
        defaultBoard.Init("h8", Some (Piece(PieceTypes.Rook, PieceColors.Black)))
        
        defaultBoard.Init("a7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("b7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("c7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("d7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("e7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("f7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("g7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard.Init("h7", Some (Piece(PieceTypes.Pawn, PieceColors.Black)))
        defaultBoard