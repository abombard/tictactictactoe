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
        | Owner ( o ) -> failwith "Error: This game is over"
        | Board ( l ) ->
            let rec aux l nl j =
                match l with
                | [] -> Board ( nl )
                | e :: tail when i = j -> aux tail (nl @ [c]) (j+1)
                | e :: tail -> aux tail (nl @ [e]) (j+1)
            in aux l [] 0

end


