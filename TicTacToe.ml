let create_list elem size =
    let rec aux i nl =
        if i = size then nl
        else aux (i+1) (elem :: nl)
    in aux 0 []

module TicTacToe =
struct

    type t = Board.t list * char

    let newTicTacToe () = (create_list (Board.newBoard ()) 9, 'O')

    let nextPlayer c = if c = 'O' then 'X' else 'O'

    let play (boards, c) x y =
        let rec aux l nl i =
            match l with
            | [] -> ( nl, nextPlayer c )
            | board :: tail when i = x -> aux tail (nl @ [Board.play board y c]) (i+1)
            | board :: tail -> aux tail (nl @ [board]) (i+1)
        in aux boards [] 0

end
