let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module Game =
struct

    type t = Board.t list * Player.t

    let newGame () = (create_list (Board.newBoard ()) 9, Player.O)

    let play (boards, player) x y =
        let nextPlayer () =
            match player with
            | Player.O -> Player.X
            | Player.X -> Player.O
            | Player.None -> Player.None
        in
        let rec aux l nl i =
            match l with
            | [] -> ( nl, nextPlayer () )
            | board :: tail when i = x -> aux tail (nl @ [Board.play board y player]) (i+1)
            | board :: tail -> aux tail (nl @ [board]) (i+1)
        in aux boards [] 0

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

end

