module Tests

open ChessBot.Entities.Board
open ChessBot.Entities.Pieces
open Xunit

[<Fact>]
let ``A1HasWhiteRookAtStart`` () =
    let chessboard = ChessBoard.Default
    let a1Square = chessboard.ChessSquares.[0, 0];  
    Assert.True(a1Square.Piece.IsSome)
    Assert.Equal(PieceColors.White, a1Square.Piece.Value.PieceColor)
    Assert.Equal(PieceTypes.Rook, a1Square.Piece.Value.PieceType)
    
    
[<Fact>]
let ``A8HasBlackRookAtStart`` () =
    let chessboard = ChessBoard.Default
    let a8Square = chessboard.ChessSquares.[0, 7];  
    Assert.True(a8Square.Piece.IsSome)
    Assert.Equal(PieceColors.Black, a8Square.Piece.Value.PieceColor)
    Assert.Equal(PieceTypes.Rook, a8Square.Piece.Value.PieceType)
