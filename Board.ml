let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module Board =
struct

    type t = Winner of Player.t
           | Board of Player.t list

    let newBoard () = Board ( create_list Player.None 9 )

    let play t i c =
        match t with
        | Winner ( _ ) -> print_endline "Error: This board game is over"; t
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Board ( nl )
                | e :: tail when i = j ->
                        if e <> Player.None
                        then begin
                            print_endline "Error: Cell already taken";
                            aux tail (nl @ [e]) (j+1)
                        end
                        else aux tail (nl @ [c]) (j+1)
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

