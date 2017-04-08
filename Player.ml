module Player =
struct

    type t = O | X | None

    let toString t =
        match t with
        | O -> "O"
        | X -> "X"
        | None -> "None"

end

