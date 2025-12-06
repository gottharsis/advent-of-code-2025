open Containers

let joltages line =
  line |> String.to_seq |> List.of_seq
  |> List.map (fun a -> Char.code a - Char.code '0')

let max_joltage joltages =
  let rec aux current_left current_right lst =
    match lst with
    | d1 :: d2 :: rest ->
        if d1 > current_left && d2 > current_right then aux d1 d2 (d2 :: rest)
        else if d1 > current_left then aux d1 d2 (d2 :: rest)
        else if d2 > current_right then aux current_left d2 (d2 :: rest)
        else aux current_left current_right (d2 :: rest)
    | _ -> (current_left, current_right)
  in
  let left, right = aux 0 0 joltages in
  (10 * left) + right

let part1 lines =
  lines |> List.map joltages |> List.map max_joltage |> List.fold_left ( + ) 0
  |> string_of_int

let max_in_subarray array start endi =
  let len = endi - start in
  array |> CCArray.to_seqi |> CCSeq.drop start |> CCSeq.take len
  |> Seq.fold_left
       (fun (besti, best) (i, elem) ->
         if elem > best then (i, elem) else (besti, best))
       (-1, -1)

(* *)
let max_joltages2 amt joltages =
  let joltages = Array.of_list joltages in
  let n = Array.length joltages in
  let rec loop num_remaining acc start =
    if Int.equal num_remaining 0 then acc
    else
      let endi = n - num_remaining + 1 in
      let new_start, digit = max_in_subarray joltages start endi in
      loop (num_remaining - 1) ((10 * acc) + digit) (new_start + 1)
  in
  loop amt 0 0

let part2 lines =
  lines
  |> List.map (fun line ->
      let arr = joltages line in
      let best = max_joltages2 12 arr in
      (* Printf.printf "For %s got %d\n" line best; *)
      best)
  |> List.fold_left ( + ) 0 |> string_of_int
