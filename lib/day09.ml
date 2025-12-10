let parse_point line =
  match line |> String.split_on_char ',' |> List.map int_of_string with
  | [ a; b ] -> (a, b)
  | _ -> raise (Common.Bad_input "Invalid tuple")

let area (x1, y1) (x2, y2) =
  let d a b = 1 + Int.abs (b - a) in
  d y2 y1 * d x2 x1

let part1 lines =
  let points = List.map parse_point lines in
  let pairs = Util.pairs points in
  Format.printf "From %d points got %d pairs@." (List.length points)
    (List.length pairs);
  pairs
  |> List.fold_left
       (fun acc corners -> Int.max acc @@ Util.uncurry2 area corners)
       (-1)
  |> string_of_int

let part2 _ = "not implemented"
