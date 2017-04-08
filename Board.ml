let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module Board =
struct

    type t = Owner of char
           | Board of char list

    let newBoard () = Board ( create_list '_' 9 )

    let play t i c =
        match t with
        | Owner ( o ) -> print_endline "Error: This board game is over"; t
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Board ( nl )
                | e :: tail when i = j ->
                        if e <> '_' then print_endline "Error: This cell is already taken";
                        aux tail (nl @ [c]) (j+1)
                | e :: tail -> aux tail (nl @ [e]) (j+1)
            in aux l [] 0

    let isOver t =
        match t with
        | Owner ( _ ) -> true
        | Board ( _ ) -> false


    let getWinner t =
        match t with
        | Owner ( c ) -> 
end

