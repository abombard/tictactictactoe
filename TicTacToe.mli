module TicTacToe :
sig

    type t

    val newTicTacToe : unit -> t

    val play : t -> int -> int -> t

    val toString : t -> string

end
