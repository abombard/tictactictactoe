let askName except =
	print_endline "Player Name ?";
	let rec getN name =
		match read_line () with
			| ""					-> print_endline "Error, empty name."; getN name
			| tmp when tmp = name	-> print_endline "Error, name already taken."; getN name
			| _	as tmpn				-> tmpn
	in getN except

(* let rec askEndGame names =
	print_endline "End, do you want ro restart ? (y/n)";
	match read_line () with
	| "y"	-> game (Game.newGame ()) 0 names
	| "n"	-> print_endline "Closing."
	| _ 	-> print_endline "Error, invalid input."; askEndGame names *)

let rec askMove name =
	print_endline (name ^ "'s turn to play.");
	let smove = String.trim (read_line ()) in
	let lmove = String.split_on_char ' ' smove in
	if (List.length lmove = 2)
	then
		try
			let move = (int_of_string (List.hd lmove), int_of_string (List.nth lmove 1)) in
			print_endline ("Input : " ^ string_of_int (fst move) ^ " " ^ string_of_int (snd move)) (* a modifier*)
		with
			int_of_string -> print_endline ("Incorrect format. (2 numbers separate by a space required)"); askMove name
	else
		print_endline ("Incorrect format. (2 numbers separate by a space required)");
		askMove name


let main () =
	let p1 = askName "" in
	let p2 = askName p1 in
	askMove p1
	(* let rec game t playerid names =
		if Game.isFinish t
		then
			askEndGame names
		else
			game (Game.playPlayer t) ((playerid + 1) mod 2) names
	in game (Game.newGame ()) 0 (p1, p2) *)

let () = main ()
