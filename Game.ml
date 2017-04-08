let () =
    let t = TicTacToe.newTicTacToe () in
    let t = TicTacToe.play t 3 3 in
    print_endline "bla"

(*
module Tic :
struct
	type t = (int * char) list

	let newTic () = []

	let play t i c =
		t @ [(i, c)]

	let isFinish t =
		false

	let getWinner t =
	(* 0 = unfinish *)
	(* 1 = O win *)
	(* 2 = X win *)
	(* 3 = equality *)
	 0

	let getCharAt t i =
		try
			snd (List.find (function x = if fst x = i then true else false) t) in
		with Not_found -> '_'

	let getFirstLine t =
		let finish = isFinish t in
		match finish with
			| 3 -> "/-\\"
			| 2 | 1 -> "\\ /"
			| 0 -> (getCharAt t 0) ^ (getCharAt t 1) ^ (getCharAt t 2)

	let getSndLine t =
		let finish = isFinish t in
		match finish with
			| 3 -> "| |"
			| 2 -> " X "
			| 1 -> " O "
			| 0 -> (getCharAt t 3) ^ (getCharAt t 4) ^ (getCharAt t 5)

	let getThdLine t =
		let finish = isFinish t in
		match finish with
			| 3 -> "\\-/"
			| 2 | 1 -> "/ \\"
			| 0 -> (getCharAt t 6) ^ (getCharAt t 7) ^ (getCharAt t 8)
end

type t = Tic.t list

let isFinish t =
	false

let getWinner t =
	(* 0 = unfinish *)
	(* 1 = O win *)
	(* 2 = X win *)
	(* 3 = equality *)
	 0

let playIA t =
	t

let playPlayer t =
	t

let printRes t =
	print_newline (Tic.getFirstLine (nth t 0) ^ "|" ^ Tic.getFirstLine (nth t 1) ^ "|" ^ Tic.getFirstLine (nth t 2) ^ "|");
	print_newline ("-----------");
	print_newline (Tic.getFirstLine (nth t 3) ^ "|" ^ Tic.getFirstLine (nth t 4) ^ "|" ^ Tic.getFirstLine (nth t 5) ^ "|");
	print_newline ("-----------");
	print_newline (Tic.getFirstLine (nth t 6) ^ "|" ^ Tic.getFirstLine (nth t 7) ^ "|" ^ Tic.getFirstLine (nth t 8) ^ "|");
*)
