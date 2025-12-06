let parse_id word =
  let lst =
    try String.split_on_char '-' word |> List.map int_of_string
    with Failure s -> raise (Common.Bad_input s)
  in
  match lst with
  | [ a; b ] -> (a, b)
  | _ -> raise (Common.Bad_input ("Could not parse " ^ word))

let parse_line line = String.split_on_char ',' line |> List.map parse_id

let is_invalid_id id =
  let s = string_of_int id in
  let len = String.length s / 2 in
  let first_half = String.sub s 0 len
  and second_half = String.sub s len (String.length s - len) in
  String.equal first_half second_half

let sum_invalid_ids_in_range is_invalid left right =
  let rec aux acc id =
    if id > right then acc
    else
      let acc' = if is_invalid id then acc + id else acc in
      aux acc' (id + 1)
  in
  aux 0 left

let part1 lines =
  lines |> List.hd |> parse_line
  |> List.map (fun (left, right) ->
      sum_invalid_ids_in_range is_invalid_id left right)
  |> List.fold_left ( + ) 0 |> string_of_int

(* A string consists of the same pattern repeated multiple times if and only if the string is a nontrivial rotation of itself. *)
let is_rotation str =
  let doubled = str ^ str in
  let len = String.length str in
  let rec aux i =
    if i >= len then false
    else
      let substr = String.sub doubled i len in
      if String.equal substr str then true else aux (i + 1)
  in
  aux 1

let is_invalid_id2 id =
  let s = string_of_int id in
  is_rotation s

let part2 lines =
  lines |> List.hd |> parse_line
  |> List.map (fun (left, right) ->
      sum_invalid_ids_in_range is_invalid_id2 left right)
  |> List.fold_left ( + ) 0 |> string_of_int
