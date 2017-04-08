let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module Board =
struct

    type owner = O | X | None

    let string_of_owner o =
        match o with
        | O -> "O"
        | X -> "X"
        | None -> failwith "Invalid Owner"

    type t = Owner of owner
           | Board of owner list

    let newBoard () = Board ( create_list None 9 )

    let play t i c =
        match t with
        | Owner ( _ ) -> print_endline "Error: This board game is over"; t
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Board ( nl )
                | e :: tail when i = j ->
                        if e <> None then print_endline "Error: This cell is already taken";
                        aux tail (nl @ [c]) (j+1)
                | e :: tail -> aux tail (nl @ [e]) (j+1)
            in aux l [] 0

    let isOver t =
        match t with
        | Owner ( _ ) -> true
        | Board ( _ ) -> false

    let toString t =
        match t with
        | Owner ( o ) -> begin
               match o with
               | O -> "/ - \\ |   | \\ - /"
               | X -> "\\   /   X   /   \\"
               | None -> failwith "Owner is None"
        end
        | Board ( l ) ->
                let rec aux l s =
                    match l with
                    | [] -> s
                    | o :: tail -> aux tail (s ^ string_of_owner o)
                in aux l ""

end

