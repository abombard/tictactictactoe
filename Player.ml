module Player =
struct

    type t = O | X

    let toString t =
        match t with
        | O -> "O"
        | X -> "X"

end
