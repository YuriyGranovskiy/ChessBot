module ChessBot.Entities.Squares

type SquareColor =
    | White
    | Black

type Rank =
    | R1 = 1
    | R2 = 2
    | R3 = 3
    | R4 = 4
    | R5 = 5
    | R6 = 6
    | R7 = 7
    | R8 = 8

type File =
    | A = 1
    | B = 2
    | C = 3
    | D = 4
    | E = 5
    | F = 6
    | G = 7
    | H = 8

type Square(file: File, rank : Rank) =
    
    member this.Rank = rank
    member this.File = file
    
    member this.Color =
        if (LanguagePrimitives.EnumToValue this.Rank % 2) = (LanguagePrimitives.EnumToValue this.File % 2) then
            Black
        else
            White

    member this.Notation =
        $"{this.File}{LanguagePrimitives.EnumToValue this.Rank}".ToLower()