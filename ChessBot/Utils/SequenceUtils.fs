module ChessBot.Utils.SequenceUtils

let onlySome(source: seq<option<_>>) =
    source |>  Seq.where (fun s -> s.IsSome) |> Seq.map (fun s -> s.Value)