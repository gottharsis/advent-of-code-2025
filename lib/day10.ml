type inp = {
  target : int; (* int with 1*)
  buttons : int list list;
  joltages : int list;
}

let parse_target target =
  let rec parse' acc i =
    if i >= String.length target - 1 then acc
    else
      let ch =
        match target.[i] with
        | '.' -> 0
        | '#' -> Int.shift_left 1 (i - 1)
        | _ ->
            raise
              (Common.Bad_input
                 (Format.sprintf "Invalid character %c" target.[i]))
      in
      parse' (Int.logor acc ch) (i + 1)
  in
  parse' 0 1

let parse_button button =
  let s = String.sub button 1 (String.length button - 2) in
  s |> String.split_on_char ',' |> List.map int_of_string

let parse_button_bitmask button =
  button |> Iter.of_list
  |> Iter.fold (fun acc elem -> Int.logor acc (Int.shift_left 1 elem)) 0

let parse_joltages joltages =
  let s = String.sub joltages 1 (String.length joltages - 2) in
  s |> String.split_on_char ',' |> List.map int_of_string

let parse_line line =
  let words = String.split_on_char ' ' line in
  let target = parse_target @@ List.hd words in
  let rec parse' acc lst =
    match lst with
    | [ x ] -> (List.rev acc, parse_joltages x)
    | x :: rest -> parse' (parse_button x :: acc) rest
    | _ -> raise (Common.Bad_input "Invalid list with 0 elements")
  in
  let buttons, joltages = parse' [] (List.tl words) in
  { target; buttons; joltages }

module IntSet = CCHashSet.Make (Int)

let steps_to_target target buttons =
  let open Queue in
  let queue = create () in
  add (0, 0) queue;
  let seen = IntSet.create 1000 in
  let rec bfs () =
    let v, dist = pop queue in
    if dist >= 10000000 then
      raise (Common.Bad_input "Exceeded 10000000 iterations")
    else if IntSet.mem seen v then bfs ()
    else if
      (* Format.printf "Parsing target %x at dist %d@." v dist; *)
      v == target
    then dist
    else begin
      IntSet.insert seen v;
      buttons |> Iter.of_list
      |> Iter.map (fun button -> Int.logxor v button)
      |> Iter.filter (fun v -> not (IntSet.mem seen v))
      |> Iter.map (fun v -> (v, dist + 1))
      |> Iter.iter (fun x -> add x queue);
      bfs ()
    end
  in
  bfs ()

let part1 lines =
  lines |> Iter.of_list |> Iter.map parse_line
  |> Iter.mapi (fun i { buttons; target; _ } ->
      let buttons = List.map parse_button_bitmask buttons in
      let steps = steps_to_target target buttons in
      let () = Format.printf "Line %d : steps : %d@." i steps in
      steps)
  |> Iter.fold ( + ) 0 |> string_of_int

(* part2 follows approach in https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/ *)

module IntMap = Hashtbl.Make (Int)

let get_parity arr =
  arr |> Iter.of_array
  |> Iter.map (fun v -> v mod 2)
  |> Iter.foldi (fun acc i b -> Int.logor acc (Int.shift_left b i)) 0

(* returns map of parity -> all subsets that achieve it *)
let button_subset_parities (buttons_bitmasks : int array) =
  let map = IntMap.create 500 in
  let n = Array.length buttons_bitmasks in
  let num_subsets = Int.shift_left 1 n in
  for subset_mask = 0 to num_subsets - 1 do
    let parity =
      buttons_bitmasks |> Iter.of_array
      |> Iter.filter_mapi (fun idx mask ->
          if Int.logand subset_mask (Int.shift_left 1 idx) > 0 then Some mask
          else None)
      |> Iter.fold Int.logxor 0
    in
    let old = IntMap.find_opt map parity |> Option.value ~default:[] in
    IntMap.add map parity (subset_mask :: old)
  done;
  map

let apply_button_mask buttons arr button_mask =
  let copy = Array.copy arr in
  for i = 0 to Array.length buttons - 1 do
    if Int.logand (Int.shift_left 1 i) button_mask > 0 then
      buttons.(i) |> List.iter (fun but -> copy.(but) <- copy.(but) - 1)
  done;
  copy

let solve joltages buttons button_parities =
  let cache =
    CCCache.lru ~eq:(Array.equal Int.equal) ~hash:CCHash.(array int) 256
  in
  (* let pp = CCArray.pp CCInt.pp in *)
  let solve' =
    CCCache.with_cache_rec cache (fun solve' arr ->
        (* Format.printf "Currently processing %a @." pp arr; *)
        if Array.for_all (Int.equal 0) arr then
          (* let () = print_endline "All 0" in *)
          0
        else
          let parity = get_parity arr in
          (* Format.printf " parity=%a@." CCInt.pp_binary parity; *)
          let button_mask_list =
            IntMap.find_opt button_parities parity |> Option.value ~default:[]
          in
          (* let () =
            Format.printf "Found %d buttons@." (List.length button_mask_list)
          in *)
          let cost =
            List.fold_left
              (fun best_cost button_mask ->
                let new_arr = apply_button_mask buttons arr button_mask in
                let new_arr = Array.map (fun v -> Int.div v 2) new_arr in
                if Array.for_all (fun x -> x >= 0) new_arr then
                  (* let () =
                    Format.printf "button_mask %a new array: %a @."
                      CCInt.pp_binary button_mask pp new_arr
                  in *)
                  let cost =
                    (2 * solve' new_arr) + CCInt.popcount button_mask
                  in
                  Int.min cost best_cost
                else best_cost)
              1000000 button_mask_list
          in
          (* Format.printf " finalcost=%d @." cost; *)
          cost)
  in
  solve' joltages

let part2 lines =
  lines |> Iter.of_list |> Iter.map parse_line
  |> Iter.mapi (fun i { buttons; joltages; _ } ->
      let joltages = Array.of_list joltages in
      let buttons = Array.of_list buttons in
      let button_bitmasks = Array.map parse_button_bitmask buttons in
      let buttons_by_parity = button_subset_parities button_bitmasks in
      let steps = solve joltages buttons buttons_by_parity in
      let () = Format.printf "part 2 Line %d : steps : %d@." i steps in
      steps)
  |> Iter.fold ( + ) 0 |> string_of_int
