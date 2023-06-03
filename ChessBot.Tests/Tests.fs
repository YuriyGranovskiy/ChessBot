module Tests

open ChessBot.Entities.Game
open ChessBot.Entities.Board
open ChessBot.Entities.Pieces
open ChessBot.Exceptions.ValidationExceptions

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``A1HasWhiteRookAtStartTest`` () =
    let chessboard = ChessBoard.Default
    let a1Square = chessboard.GetByCode("a1");
    a1Square.Piece.IsSome |> should be True
    a1Square.Piece.Value.PieceColor |> should equal White
    a1Square.Piece.Value.PieceType |> should equal Rook
    
    
[<Fact>]
let ``A8HasBlackRookAtStartTest`` () =
    let chessboard = ChessBoard.Default
    let a8Square = chessboard.GetByCode("a8");  
    a8Square.Piece.IsSome |> should be True
    a8Square.Piece.Value.PieceColor |> should equal Black
    a8Square.Piece.Value.PieceType |> should equal Rook

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
    e2Square.Piece.IsNone |> should be True
    e7Square.Piece.IsNone |> should be True
    e4Square.Piece.IsSome |> should be True
    e4Square.Piece.Value.PieceColor |> should equal White
    e4Square.Piece.Value.PieceType |> should equal Pawn
    e5Square.Piece.IsSome |> should be True
    e5Square.Piece.Value.PieceColor |> should equal Black
    e5Square.Piece.Value.PieceType |> should equal Pawn
    
    
[<Fact>]
let ``TwoSquareWhitePawnImpossibleWithObstacleTest`` () =
    let chessboard = ChessBoard()
    let e2Square = chessboard.GetByCode("e2")
    e2Square.Piece <- Some (Piece(Pawn, White))
    let e3Square = chessboard.GetByCode("e3")
    e3Square.Piece <- Some (Piece(Bishop, White))
    let game = ChessGame(chessboard)
    (fun () -> game.Move "e4") |> should throw typeof<ImpossibleMove>
    
[<Fact>]
let ``TwoSquareBlackPawnImpossibleWithObstacleTest`` () =    
    let chessboard = ChessBoard()
    let e2Square = chessboard.GetByCode("e2")
    e2Square.Piece <- Some (Piece(Pawn, White))
    let e7Square = chessboard.GetByCode("e7")
    e7Square.Piece <- Some (Piece(Pawn, Black))
    let e6Square = chessboard.GetByCode("e6")
    e6Square.Piece <- Some (Piece(Bishop, Black))
    let game = ChessGame(chessboard)
    game.Move "e3"
    (fun () -> game.Move "e5") |> should throw typeof<ImpossibleMove>

[<Fact>]
let ``WhitePawnImpossibleTwoSquareFromNonSecondRankTest`` () =    
    let chessboard = ChessBoard()
    let e2Square = chessboard.GetByCode("e3")
    e2Square.Piece <- Some (Piece(Pawn, White))
    let game = ChessGame(chessboard)
    (fun () -> game.Move "e5") |> should throw typeof<ImpossibleMove>

[<Fact>]
let ``BlackPawnImpossibleTwoSquareFromNonSeventhRankTest`` () =
    let chessboard = ChessBoard()
    let e2Square = chessboard.GetByCode("d2")
    e2Square.Piece <- Some (Piece(Pawn, White))
    let e2Square = chessboard.GetByCode("e6")
    e2Square.Piece <- Some (Piece(Pawn, Black))
    let game = ChessGame(chessboard)
    game.Move "d3"
    (fun () -> game.Move "e4") |> should throw typeof<ImpossibleMove>

[<Fact>]
let ``KnightCanGoF3Test`` () =
    let chessboard = ChessBoard.Default
    let game = ChessGame(chessboard)
    game.Move "Nf3"
    chessboard.GetByCode("g1").Piece.IsNone |> should be True
    chessboard.GetByCode("f3").Piece.IsSome |> should be True
    
[<Fact>]
let ``TwoKnightsCantGoToTheSameSquareTest`` () =
    let chessboard = ChessBoard()
    let c3Square = chessboard.GetByCode("c3")
    c3Square.Piece <- Some (Piece(Knight, White))
    let g3Square = chessboard.GetByCode("g3")
    g3Square.Piece <- Some (Piece(Knight, White))
    let game = ChessGame(chessboard)
    (fun () -> game.Move "Ne4") |> should throw typeof<ImpossibleMove>
    
[<Fact>]
let ``TwoKnightsOfDiffColorCanChooseProperOneToTheSameSquareTest`` () =
    let chessboard = ChessBoard()
    let c3Square = chessboard.GetByCode("c3")
    c3Square.Piece <- Some (Piece(Knight, White))
    let g3Square = chessboard.GetByCode("g3")
    g3Square.Piece <- Some (Piece(Knight, Black))
    let game = ChessGame(chessboard)
    game.Move "Ne4"
    c3Square.Piece.IsNone |> should be True