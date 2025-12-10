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

let print_int_pair (a, b) = Format.printf "(%d, %d)@." a b
let int_of_bool b = if b then 1 else 0

let array_pairs arr =
  let n = Array.length arr in
  let rec outer i acc =
    if i >= n - 1 then acc
    else
      let rec inner j acc =
        if j >= n then acc else inner (j + 1) ((arr.(i), arr.(j)) :: acc)
      in
      outer (i + 1) (inner (i + 1) acc)
  in
  List.rev @@ outer 0 []

(* [pairs list] return a list of all unordered 2 pairs of a list *)

let pairs list = array_pairs @@ Array.of_list list
let uncurry2 f (a, b) = f a b
