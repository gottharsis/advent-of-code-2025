let parse_range word =
  match String.split_on_char '-' word with
  | [ l; r ] -> (int_of_string l, int_of_string r)
  | _ -> raise (Common.Bad_input ("Can't parse range " ^ word))

let parse_data lines =
  let seq = List.to_seq lines in
  let ranges =
    seq
    |> Seq.take_while (fun line -> String.length line > 0)
    |> Seq.map parse_range |> Array.of_seq
  and ids =
    seq
    |> Seq.drop_while (fun line -> String.length line > 0)
    |> Seq.drop 1 |> Seq.map int_of_string
  in
  (ranges, ids)

let contains id (l, r) = l <= id && id <= r

(** Binatry search for whether it's valid or not *)
let is_fresh ranges id = ranges |> Array.to_seq |> Seq.exists (contains id)

let part1 lines =
  let ranges, ids = parse_data lines in
  ids
  |> Seq.map (fun id -> is_fresh ranges id |> Util.int_of_bool)
  |> Seq.fold_left ( + ) 0 |> string_of_int

let merge (l1, r1) (l2, r2) =
  if l1 <= r2 && l2 <= r1 then (* overlap *)
    Some (Int.min l1 l2, Int.max r1 r2)
  else None

let merge_overlapping_ranges (ranges : (int * int) array) =
  Array.sort (fun (l1, _) (l2, _) -> compare l1 l2) ranges;
  let ranges = Array.to_seq ranges in
  let rec visit current unmerged () =
    match unmerged () with
    | Seq.Nil -> Seq.Cons (current, Seq.empty)
    | Seq.Cons (interval, rest) -> (
        match merge current interval with
        | Some new_current -> visit new_current rest ()
        | None -> Seq.Cons (current, visit interval rest))
  in
  match ranges () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (value, rest) -> visit value rest

let part2 _lines =
  let ranges, _ = parse_data _lines in
  merge_overlapping_ranges ranges
  |> Seq.map (fun (l, r) -> r - l + 1)
  |> Seq.fold_left ( + ) 0 |> string_of_int
