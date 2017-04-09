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

let toInt t =
    match t with
    | None -> 0
    | O -> 1
    | X -> 2

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

let lineIsComplete board i =
    let getLine i = match i with
        | 1 -> [List.nth board 0; List.nth board 1; List.nth board 2]
        | 2 -> [List.nth board 3; List.nth board 4; List.nth board 5]
        | 3 -> [List.nth board 6; List.nth board 7; List.nth board 8]
        | _ -> []
    in
    let n = getLine i in
    if (List.nth n 0 <> Player.None && List.nth n 0 = List.nth n 1 && List.nth n 1 = List.nth n 2)
    then Winner (List.nth n 0)
    else Board (board)

let columnIsComplete board i =
    let n = match i with
    | 1 -> [List.nth board 0; List.nth board 3; List.nth board 6]
    | 2 -> [List.nth board 1; List.nth board 4; List.nth board 7]
    | 3 -> [List.nth board 2; List.nth board 5; List.nth board 8]
    | _ -> []
    in
    if (List.nth n 0 <> Player.None && List.nth n 0 = List.nth n 1 && List.nth n 1 = List.nth n 2)
    then Winner (List.nth n 0)
    else Board (board)

let diagonaleIsComplete board i =
    let n = match i with
    | 1 -> [List.nth board 0; List.nth board 4; List.nth board 8]
    | 2 -> [List.nth board 6; List.nth board 4; List.nth board 2]
    | _ -> []
    in
    if (List.nth n 0 <> Player.None && List.nth n 0 = List.nth n 1 && List.nth n 1 = List.nth n 2)
    then Winner (List.nth n 0)
    else Board (board)

let allTestFunc = [lineIsComplete; lineIsComplete; lineIsComplete; columnIsComplete;
    columnIsComplete; columnIsComplete; diagonaleIsComplete; diagonaleIsComplete]

let rec checkFull board i =
    if i > 8 then true
    else
        match (List.nth board i) with
        | Player.None -> false
        | _ -> checkFull board (i + 1)

let isFinish (board : Player.t list) =
    let rec test board i =
        if i >= List.length allTestFunc then begin
			if not (checkFull board 0)
			then None
			else Some Player.None
		end
        else
            let boardres = (List.nth allTestFunc i) board (i mod 3 + 1) in
            match boardres with
            | Winner ( p ) -> Some ( p )
            | Board ( l ) -> test board (i + 1)
    in test board 0

let finish (board : t) (player : Player.t) =
	match board with
	| Board l -> begin
    	match isFinish l with
    	| Some p when p = Player.None -> Winner player
        | Some p                      -> Winner p
    	| None 		                  -> board
	end
	| _      -> board

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

let play ((boards : Board.t list), (player : Player.t)) x y =
    if x < 0 || x > 8 || y < 0 || y > 8
    then Error "Illegal move."
    else begin
        let nextPlayer () =
            match player with
            | Player.O -> Player.X
            | Player.X -> Player.O
            | Player.None -> Player.None
        in
        let rec aux (l : Board.t list) (nl : Board.t list) i =
            match l with
            | [] -> Some ( nl, nextPlayer () )
            | board :: tail when i = x -> begin
                match Board.play board y player with
                | Some ( newBoard ) -> aux tail (nl @ [Board.finish newBoard player]) (i+1)
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

    let isFinish (boards, _) =
        let rec test boards res =
            match boards with
            | [] -> begin
                let rec aux i =
                    if i >= List.length Board.allTestFunc then None
                    else begin
                        let boardres = (List.nth Board.allTestFunc i) res (i mod 3 + 1) in
                        match boardres with
                        | Board.Winner ( p ) -> Some ( p )
                        | Board.Board ( l ) -> aux (i + 1)
                    end
                in aux 0
            end
            | Board.Board ( board ) :: tail -> begin
                match Board.isFinish board with
                | Some ( p ) -> test tail (res @ [p])
                | _          -> test tail (res @ [Player.None])
            end
            | Board.Winner ( p ) :: tail -> test tail (res @ [p])
        in test boards []

    let ai (boards, _) =
        let playerAi = Player.X in
        (* let playerHuman = Player.O in *)
        let rec findFreeCell line =
            let rec aux l =
                match l with
                | ( p, x, y ) :: tail when p = Player.None -> (x, y)
                | cell :: tail                             -> findFreeCell tail
                | []                                       -> (-1, -1)
            in aux line
        in
        let estimateLineValue line player =
            let rec aux l v =
                match l with
                | []                                       -> v
                | ( p, _, _ ) :: tail when p = Player.None -> aux tail v
                | ( p, _, _ ) :: tail when p = player      -> aux tail (v + 1)
                | ( p, _, _ ) :: tail                      -> aux tail (v - 5)
            in aux line 0
        in
        let searchCell line player =
            match estimateLineValue line player with
            | x when x <> 2 && x <> -10 -> None
            | _                         -> Some ( findFreeCell line )
        in
        let allLinesY = [
            (0, 1, 2);
            (3, 4, 5);
            (6, 7, 8);
            (0, 3, 6);
            (1, 4, 7);
            (2, 5, 8);
            (0, 4, 8);
            (2, 4, 6);
        ]
        in
        let getNextMove boards =
            let rec boards_iter1 boards x =
                match boards with
                | Board.Winner p :: tail  -> boards_iter1 tail (x+1)
                | Board.Board board :: tail -> begin
                    let rec lines_iter lines =
                        match lines with
                        | [] -> None
                        | ( i, j, k ) :: tail -> begin
                            let l = [(List.nth board i, x, i ); ( List.nth board j, x, j); ( List.nth board k, x, k )] in
                            match searchCell l playerAi with
                            | Some (x, y) -> Some (x, y)
                            | None        -> lines_iter tail
                        end
                    in
                    match lines_iter allLinesY with
                    | Some (x, y) -> (x, y)
                    | None        -> boards_iter1 tail (x+1)
                end
                | [] -> (-1, -1)
            in
            match boards_iter1 boards 1 with
            | (-1, -1) -> begin
                let rec boards_iter2 boards x =
                    match boards with
                    | []                        -> (-1, -1)
                    | Board.Winner p :: tail    -> print_endline "Winner !"; boards_iter2 tail (x+1)
                    | Board.Board board :: tail -> begin
                        print_endline "Board !";
                        let rec board_iter cells y =
                            match cells with
                            | []                                   -> None
                            | cell :: tail when cell = Player.None -> Some (x, y)
                            | cell :: tail                         -> board_iter tail (y+1)
                        in
                        match board_iter board 0 with
                        | Some (x, y) -> (x, y+1)
                        | None        -> boards_iter2 tail (x+1)
                    end
                in boards_iter2 boards 1
            end
            | (x, y) -> (x, y+1)
       in getNextMove boards

end

(* main *)

let askName except =
print_endline "Player Name ?";
let rec getN name =
    match read_line () with
        | ""								-> print_endline "Error: empty name."; getN name
        | "AI" as tmp | tmp when tmp = name	-> print_endline "Error: name already taken."; getN name
        | name  							-> name
in getN except

let rec askNbPlayer () =
	print_endline "Number of player ? (1 or 2)";
	match read_line () with
		| "1"	-> print_endline "1 player selected."; 1
		| "2"	-> print_endline "2 player selected."; 2
		| _		-> print_endline "Invalid input."; askNbPlayer ()

let askMove player =
    print_endline (player ^ "'s turn to play.")

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

let rec askEndGame names =
    print_endline "End, do you want ro restart ? (y/n)";
    match read_line () with
    | "y"	-> 0
    | "n"	-> 1
    | _ 	-> print_endline "Error, invalid input."; askEndGame names

let rec getCoordonates game player =
    askMove player;
    (* let inputs = split_whitespaces (String.trim (read_line ())) in *)
    match player with
    | "AI" -> Game.ai game
    | _    -> begin
        try
            let inputs = String.split_on_char ' ' (String.trim (read_line ())) in
            match inputs with
            | x :: y :: [] -> (int_of_string x, int_of_string y)
            | _            ->
		        print_endline "Error: Incorrect format. Usage: x y";
                getCoordonates game player
	    with
		    int_of_string ->
		        print_endline "Error: Incorrect format. Usage: x y";
                getCoordonates game player
    end

let rec game_loop game names idplayer =
    let (x, y) = getCoordonates game (List.nth names idplayer) in
    match Game.play game (x-1) (y-1) with
    | Some ( newGame ) -> begin
		print_endline (Game.toString newGame);
        match Game.isFinish newGame with
        | Some ( p )	-> begin
            match (askEndGame names) with
                | 0 -> game_loop (Game.newGame ()) names 0
                | _ -> print_endline "Closing."
        end
        | _ 		    -> game_loop newGame names ((idplayer + 1) mod 2)
	end
    | Error ( message ) ->
            print_string "Error: ";
            print_endline message;
            game_loop game names idplayer

let main () =
    let game = Game.newGame () in
	let nbplayer = askNbPlayer () in
    let name1 = askName "" in
	let name2 = match nbplayer with
				| 1 -> "AI"
				| 2 -> askName name1
				| _ -> "Error"
	in
    print_string name1;
    print_string " vs ";
    print_endline name2;
    game_loop game [name1; name2] 0

let () = main ()
