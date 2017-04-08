let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module type Board =
struct

    type t = Winner of Player.t
           | Board of (Some player) list

    let newBoard () = Board ( create_list None 9 )

    let play t i c =
        match t with
        | Winner ( _ ) -> print_endline "Error: This board game is over"; t
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Board ( nl )
                | e :: tail when i = j ->
                        aux tail (nl @ [c]) (j+1)
                | e :: tail -> aux tail (nl @ [e]) (j+1)
            in aux l [] 0

    let toString t =
        match t with
        | Winner ( opt ) -> begin
               match opt with
               | Some ( player ) with player = Player.O -> "/ - \\ |   | \\ - /"
               | Some ( player ) with player = Player.X -> "\\   /   X   /   \\"
               | None -> failwith "Board Winner is None"
        end
        | Board ( l ) ->
                let rec aux l s =
                    match l with
                    | [] -> s
                    | o :: tail -> aux tail (s ^ string_of_owner o)
                in aux l ""

end

