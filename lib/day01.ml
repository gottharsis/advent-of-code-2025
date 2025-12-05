(* lib/day01.ml *)

type rotation =
| Left of int
| Right of int

let amt rot = match rot with
| Left v -> v
| Right v -> v
let parse_rotation rot =
  let direction = rot.[0] in
  let distance = int_of_string (String.sub rot 1 (String.length rot - 1)) in
  match direction  with
  | 'L' ->  Some (Left distance)
  | 'R' -> Some (Right distance)
  | _ -> None

let apply_rotation pos rot = pos + match rot with
| Left v -> Int.neg v
| Right v -> v

let (%) x y =
  let z = x mod y in
  if z < 0 then z + y else z

let part1 lines =
  let fold_func (acc: int * int) (rotation) =
    let pos, zeros = acc in
    let new_pos = (apply_rotation pos rotation) % 100 in
    let new_zeros = if Int.equal new_pos 0 then zeros + 1 else zeros in
    (new_pos, new_zeros)

  in
  let initial_state = (50, 0) in

  match List.map parse_rotation lines |> Util.all_some  with
  | None -> "Failed to parse"
  | Some rotations ->
    let (_, zeros) = List.fold_left fold_func initial_state rotations in
    string_of_int zeros


let int_of_bool b = if b then 1 else 0

let part2 lines =
  let fold_func (pos, zeros) (rotation: rotation) =
    let amount = amt rotation in
    if amount == 0 then (pos, zeros)
    else
    let is_left = match rotation with
    | Left _ -> true
    | Right _ -> false
   in
       let loops = amount / 100 in
       let amount_in_one_rotation = amount mod 100 in
       if is_left then
        let temp_new_pos = pos - amount_in_one_rotation in
        let crossed = if (pos <> 0) && (temp_new_pos <= 0) then 1 else 0 in
        let new_pos = temp_new_pos % 100 in
        (new_pos, zeros + loops + crossed)
       else
        let temp_new_pos = pos + amount_in_one_rotation in
        let crossed = if (pos <> 0) && (temp_new_pos >= 100) then 1 else 0 in
        let new_pos = temp_new_pos % 100 in
        (new_pos, zeros + loops + crossed)
  in
  let initial_state = (50, 0) in

  match List.map parse_rotation lines |> Util.all_some  with
  | None -> "Failed to parse"
  | Some rotations ->
    let (_, zeros) = List.fold_left fold_func initial_state rotations in
    string_of_int zeros
