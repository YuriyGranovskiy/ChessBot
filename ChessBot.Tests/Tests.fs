module Tests

open ChessBot.Entities.Game
open ChessBot.Entities.Board
open ChessBot.Entities.Pieces
open Xunit

[<Fact>]
let ``A1HasWhiteRookAtStartTest`` () =
    let chessboard = ChessBoard.Default
    let a1Square = chessboard.GetByCode("a1");
    Assert.True(a1Square.Piece.IsSome)
    Assert.Equal(PieceColors.White, a1Square.Piece.Value.PieceColor)
    Assert.Equal(PieceTypes.Rook, a1Square.Piece.Value.PieceType)
    
    
[<Fact>]
let ``A8HasBlackRookAtStartTest`` () =
    let chessboard = ChessBoard.Default
    let a8Square = chessboard.GetByCode("a8");  
    Assert.True(a8Square.Piece.IsSome)
    Assert.Equal(PieceColors.Black, a8Square.Piece.Value.PieceColor)
    Assert.Equal(PieceTypes.Rook, a8Square.Piece.Value.PieceType)

[<Fact>]
let ``KingsPawnOpeningMovesPawnsTest`` () =
    let chessboard = ChessBoard.Default
    let game = ChessGame(chessboard)
    game.Move("e4")
    game.Move("e5")
    let e2Square = chessboard.GetByCode("e2");
    let e4Square = chessboard.GetByCode("e4")
    let e5Square = chessboard.GetByCode("e5");
    let e7Square = chessboard.GetByCode("e7");
    Assert.True(e2Square.Piece.IsNone, "Pawn on e2 exists but shouldn't")
    Assert.True(e7Square.Piece.IsNone, "Pawn on e7 exists but shouldn't")
    Assert.True(e4Square.Piece.IsSome, "Pawn on e4 doesn't exist but should")
    Assert.Equal(White, e4Square.Piece.Value.PieceColor)
    Assert.Equal(Pawn, e4Square.Piece.Value.PieceType)
    Assert.True(e5Square.Piece.IsSome, "Pawn on e5 doesn't exist but should")
    Assert.Equal(Black, e5Square.Piece.Value.PieceColor)
    Assert.Equal(Pawn, e5Square.Piece.Value.PieceType)
    
    