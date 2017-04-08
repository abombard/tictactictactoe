let main () =
	let n = ("Player 1", "Player 2")
	let rec game t playerid names =
		if Game.isFinish t
		then
		begin
			print_endline "End, do you want ro restart ? (y/n)";
			let rec restart () =
			match read_line with
			| "y"	-> game (Game.newGame ()) 0 names
			| "n"	-> print_endline "Closing..."
			| _ 	-> print_endline "Error, invalid input, do you want ro restart ? (y/n)"
			in restart ()
		end
		else
			game (Game.playPlayer t) ((playerid + 1) mod 2) names


	in game (Game.newGame ()) 0

let () = main ()
