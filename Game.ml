let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

(* Error handline *)
type 'a option =  Some of 'a
                | Error of string

(* Player *)

module Player =
struct

    type t = O | X | None

    let toString t =
        match t with
        | O -> "O"
        | X -> "X"
        | None -> "_"

end

(* Board *)

module Board =
struct

    type t = Winner of Player.t
           | Board of Player.t list

    let newBoard () = Board ( create_list Player.None 9 )

    let play t i c =
        match t with
        | Winner ( _ ) -> Error "Illegal move."
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Some ( Board ( nl ) )
                | e :: tail when i = j ->
                        if e <> Player.None
                        then Error "Illegal move."
                        else aux tail (nl @ [c]) (j+1)
						(* Check if board is over au dessus*)
                | e :: tail -> aux tail (nl @ [e]) (j+1)
            in aux l [] 0

    let toString t =
        match t with
        | Winner ( player ) -> begin
               match player with
               | Player.O -> "/ - \\ |   | \\ - /"
               | Player.X -> "\\   /   X   /   \\"
               | Player.None -> "None"
        end
        | Board ( l ) ->
                let rec aux l s =
                    match l with
                    | [] -> s
                    | player :: [] -> s ^ Player.toString player
                    | player :: tail -> aux tail (s ^ Player.toString player ^ " ")
                in aux l ""

end

(* Game *)

module Game =
struct

    type t = Board.t list * Player.t

    let newGame () =
		(create_list (Board.newBoard ()) 9, Player.O)

    let play (boards, player) x y =
        if x < 0 || x > 8 || y < 0 || y > 8
        then Error "Illegal move."
        else begin
            let nextPlayer () =
                match player with
                | Player.O -> Player.X
                | Player.X -> Player.O
                | Player.None -> Player.None
            in
            let rec aux l nl i =
                match l with
                | [] -> Some ( nl, nextPlayer () )
                | board :: tail when i = x -> begin
                    match Board.play board y player with
                    | Some ( newBoard ) -> aux tail (nl @ [newBoard]) (i+1)
                    | Error ( message ) -> Error message
                end
                | board :: tail -> aux tail (nl @ [board]) (i+1)
            in aux boards [] 0
        end

    let toString ( boards, player ) =
        let board0 = Board.toString (List.nth boards 0) in
        let board1 = Board.toString (List.nth boards 1) in
        let board2 = Board.toString (List.nth boards 2) in
        let board3 = Board.toString (List.nth boards 3) in
        let board4 = Board.toString (List.nth boards 4) in
        let board5 = Board.toString (List.nth boards 5) in
        let board6 = Board.toString (List.nth boards 6) in
        let board7 = Board.toString (List.nth boards 7) in
        let board8 = Board.toString (List.nth boards 8) in
        let line_nth board n =
            match n with
            | 1 -> String.sub board 0 5
            | 2 -> String.sub board 6 5
            | 3 -> String.sub board 12 5
            | _ -> "Error"
        in
        (line_nth board0 1) ^ " | " ^ (line_nth board1 1) ^ " | " ^ (line_nth board2 1) ^ "\n" ^
        (line_nth board0 2) ^ " | " ^ (line_nth board1 2) ^ " | " ^ (line_nth board2 2) ^ "\n" ^
        (line_nth board0 3) ^ " | " ^ (line_nth board1 3) ^ " | " ^ (line_nth board2 3) ^ "\n" ^
        "---------------------" ^ "\n" ^
        (line_nth board3 1) ^ " | " ^ (line_nth board4 1) ^ " | " ^ (line_nth board5 1) ^ "\n" ^
        (line_nth board3 2) ^ " | " ^ (line_nth board4 2) ^ " | " ^ (line_nth board5 2) ^ "\n" ^
        (line_nth board3 3) ^ " | " ^ (line_nth board4 3) ^ " | " ^ (line_nth board5 3) ^ "\n" ^
        "---------------------" ^ "\n" ^
        (line_nth board6 1) ^ " | " ^ (line_nth board7 1) ^ " | " ^ (line_nth board8 1) ^ "\n" ^
        (line_nth board6 2) ^ " | " ^ (line_nth board7 2) ^ " | " ^ (line_nth board8 2) ^ "\n" ^
        (line_nth board6 3) ^ " | " ^ (line_nth board7 3) ^ " | " ^ (line_nth board8 3) ^ "\n"

	(* let isFinish (boards, player) =
		| isFinish (List.nth boards 0) = isFinish (List.nth boards 1)  *)

end

(* main *)

let askName except =
	print_endline "Player Name ?";
	let rec getN name =
		match read_line () with
			| ""					-> print_endline "Error: empty name."; getN name
			| tmp when tmp = name	-> print_endline "Error: name already taken."; getN name
			| name  				-> name
	in getN except

let askMove names idplayer =
	print_endline (List.nth names idplayer ^ "'s turn to play.")

(* //TEMP use String.split_on_char *)
(* let list_from_string s =
    let rec aux i l =
        if i < 0 then l
        else aux (i-1) (s.[i] :: l) in
    aux (String.length s - 1) []

let split_whitespaces s =
    let rec aux s w l =
        match s with
        | [] -> if String.length w > 0 then l @ [w] else l
        | c :: tail when c = ' ' ->
                if String.length w > 0
                then aux tail "" (l @ [w])
                else aux tail "" l
        | c :: tail -> aux tail (w ^ String.make 1 c) l
    in aux (list_from_string s) "" [] *)
(* *)

let rec game_loop game names idplayer =
    askMove names idplayer;
	(* let inputs = split_whitespaces (String.trim (read_line ())) in *)
	let inputs = String.split_on_char ' ' (String.trim (read_line ())) in
	match inputs with
	| x :: y :: [] -> begin
		try
		    let x, y = int_of_string x, int_of_string y in
            match Game.play game (x-1) (y-1) with
            | Some ( newGame ) ->
                    let game = newGame in
		            print_endline (Game.toString game);
            	    game_loop game names ((idplayer + 1) mod 2)
            | Error ( message ) ->
                    print_string "Error: ";
                    print_endline message;
            	    game_loop game names idplayer
		with
			int_of_string ->
			    print_endline "Error: Incorrect format. Usage: x y";
	            game_loop game names idplayer
	    end
    | _ ->
        print_endline "Error: Incorrect format. Usage: x y";
        game_loop game names idplayer

let main () =
    let game = Game.newGame () in
    let name1 = askName "" in
    let name2 = askName name1 in
    print_string name1;
    print_string " vs ";
    print_endline name2;
    game_loop game [name1; name2] 0

let () = main ()
