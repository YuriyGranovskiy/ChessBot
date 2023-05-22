module ChessBot.Helpers.Notation

let GetFileIndexByChar(asciiChar: char) : int =        
        let ascii = asciiChar |> int 
        if ascii >= 97 && ascii <=104 then
            ascii - 97
        else if ascii >= 65 && ascii <=72 then
            ascii - 65
        else
            raise ( System.ArgumentException($"Improper file char {asciiChar}"))

let GetRankIndexByChar(asciiChar: char) : int =
        let ascii = asciiChar |> int 
        if ascii >= 49 && ascii <=56 then
            ascii - 49
        else
            raise ( System.ArgumentException($"Improper rank char {asciiChar}"))