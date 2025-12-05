(* Define let* for Option monad *)
let ( let* ) = Option.bind

(* 'a option list -> 'a list option *)
let all_some lst =
  let rec aux = function
    | [] -> Some []
    | Some x :: xs ->
        let* rest = aux xs in
        Some (x :: rest)
    | None :: _ -> None
  in
  aux lst
