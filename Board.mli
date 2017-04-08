module Board :
sig

    type t

    val newBoard : unit -> t

    val play : t -> int -> Player.t -> t

    val toString : t -> string

end
