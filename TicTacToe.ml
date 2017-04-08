let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module TicTacToe =
struct

    type t = Board.t list * char

    let newTicTacToe () = (create_list (Board.newBoard ()) 9, 'O')

    let play (boards, c) x y =
        let nextPlayer c = if c = 'O' then 'X' else 'O' in
        let rec aux l nl i =
            match l with
            | [] -> ( nl, nextPlayer c )
            | board :: tail when i = x -> aux tail (nl @ [Board.play board y c]) (i+1)
            | board :: tail -> aux tail (nl @ [board]) (i+1)
        in aux boards [] 0

    let toString t =
        let board0 = Board.toString (List.nth t 0) in
        let board1 = Board.toString (List.nth t 1) in
        let board2 = Board.toString (List.nth t 2) in
        let board3 = Board.toString (List.nth t 3) in
        let board4 = Board.toString (List.nth t 4) in
        let board5 = Board.toString (List.nth t 5) in
        let board6 = Board.toString (List.nth t 6) in
        let board7 = Board.toString (List.nth t 7) in
        let board8 = Board.toString (List.nth t 8) in
        let line_nth board n =
            match n with
            | 1 -> String.sub board 0 6
            | 2 -> String.sub board 6 6
            | 3 -> String.sub board 12 18
            | _ -> invalid_arg (string_of_int n)
        in
        (line_nth board0 1) ^ "| " ^ (line_nth board1 1) ^ "| " ^ (line_nth board2 1) ^ "\n" ^
        (line_nth board0 2) ^ "| " ^ (line_nth board1 2) ^ "| " ^ (line_nth board2 2) ^ "\n" ^
        (line_nth board0 3) ^ "| " ^ (line_nth board1 3) ^ "| " ^ (line_nth board2 3) ^ "\n" ^
        "---------------------" ^ "\n" ^
        (line_nth board3 1) ^ "| " ^ (line_nth board4 1) ^ "| " ^ (line_nth board5 1) ^ "\n" ^
        (line_nth board3 2) ^ "| " ^ (line_nth board4 2) ^ "| " ^ (line_nth board5 2) ^ "\n" ^
        (line_nth board3 3) ^ "| " ^ (line_nth board4 3) ^ "| " ^ (line_nth board5 3) ^ "\n" ^
        "---------------------" ^ "\n" ^
        (line_nth board6 1) ^ "| " ^ (line_nth board7 1) ^ "| " ^ (line_nth board8 1) ^ "\n" ^
        (line_nth board6 2) ^ "| " ^ (line_nth board7 2) ^ "| " ^ (line_nth board8 2) ^ "\n" ^
        (line_nth board6 3) ^ "| " ^ (line_nth board7 3) ^ "| " ^ (line_nth board8 3) ^ "\n"

end
