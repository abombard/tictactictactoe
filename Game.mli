module Tic :
sig
	type t

	val newTic : unit -> t

	val play : t int char -> t

	val isFinish : t -> bool
	val getWinner : t -> int


	val getCharAt : t int -> char

	val getFirstLine : t -> string
	val getSndLine : t -> string
	val getThdLine : t -> string
end

type t

val newGame : unit -> t

val isFinish : t-> bool
val getWinner : t -> int

val playIA : t -> t
val playPlayer : t -> t

val printRes : t -> ()
