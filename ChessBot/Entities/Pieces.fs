module ChessBot.Entities.Pieces

open ChessBot.DTO.Dto

type Piece(pieceType : PieceTypes, pieceColor : PieceColors) =
    member this.PieceType = pieceType
    member this.PieceColor = pieceColor
    
    member this.Describe() = $"This is a %s{this.PieceColor.ToString()} %s{this.PieceType.ToString()}" 
