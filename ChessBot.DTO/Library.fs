namespace ChessBot.DTO

open System.Text.Json.Serialization

module Dto =
    [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.InternalTag)>]
    type PieceColors =
    | White
    | Black

    [<JsonFSharpConverter(BaseUnionEncoding = JsonUnionEncoding.InternalTag)>]
    type PieceTypes =
        | Pawn
        | Knight
        | Bishop
        | Rook
        | Queen
        | King

    type PiecePositionDto = {
        Color: PieceColors
        Type: PieceTypes
        Position: string
    }


    type GameDto = {
        Move: int
        CurrentColorMove: PieceColors
        PiecePositions: PiecePositionDto[]
    }
