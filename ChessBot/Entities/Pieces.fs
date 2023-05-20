module ChessBot.Entities.Pieces

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

type Piece(pieceType : PieceTypes, pieceColor : PieceColors) =
    member this.PieceType = pieceType
    member this.PieceColor = pieceColor
    
    member this.Describe() = $"This is a %s{this.PieceColor.ToString()} %s{this.PieceType.ToString()}" 
