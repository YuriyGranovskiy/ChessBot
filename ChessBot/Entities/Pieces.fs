module ChessBot.Entities.Pieces
open ChessBot.Entities.Squares

type PieceColors =
    | White
    | Black

type PieceTypes =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Piece(pieceType : PieceTypes, pieceColor : PieceColors, square: Square) =
    member this.PieceType = pieceType
    member this.Square = square
    member this.PieceColor = pieceColor
    
    member this.Describe() = $"This is %s{this.PieceColor.ToString()} %s{this.PieceType.ToString()} on %s{this.Square.Color.ToString()} square" 
