namespace ChessBot.API.Controllers

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open ChessBot.Entities.Board
open ChessBot.Entities.Game

[<ApiController>]
[<Route("[controller]")>]
type GameController (logger : ILogger<GameController>) =
    inherit ControllerBase()

    [<HttpGet>]
    member _.Get() =
        let chessBoard = ChessBoard.Default
        let game = ChessGame(chessBoard)
        $"Move number: {game.MoveNumber}"