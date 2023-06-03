module ChessBot.Console.Helpers.ConsoleHelper

open System

let readTheMove(): string =
    let mutable result = ""
    let mutable key: ConsoleKeyInfo = ConsoleKeyInfo()
    while key.KeyChar <> '\t' && key.KeyChar <> ' ' do
        key <- Console.ReadKey()
        if key.KeyChar <> '\t' && key.KeyChar <> ' ' then result <- $"{result}{key.KeyChar}"
    result