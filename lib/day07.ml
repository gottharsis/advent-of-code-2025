let part1 lines =
  let rows = List.map CCString.to_array lines in
  match rows with
  | [] -> raise (Common.Bad_input "Empty input")
  | init :: rest ->
      let width = Array.length init in
      let initial_positions = Array.map (fun c -> c == 'S') init in
      List.fold_left
        (fun (old_pos, split_count) row ->
          let new_pos = Array.make width false in
          let current_row_split_count = ref 0 in

          for i = 0 to width - 1 do
            if old_pos.(i) then
              match row.(i) with
              | '.' -> new_pos.(i) <- true
              | '^' ->
                  current_row_split_count := !current_row_split_count + 1;
                  if i > 0 then new_pos.(i - 1) <- true;
                  if i + 1 < width then new_pos.(i + 1) <- true
              | ch ->
                  raise
                    (Common.Bad_input
                       (Format.sprintf "Invalid character : %c" ch))
          done;
          (new_pos, split_count + !current_row_split_count))
        (initial_positions, 0) rest
      |> snd |> string_of_int

let part2 lines =
  let rows = List.map CCString.to_array lines in
  match rows with
  | [] -> raise (Common.Bad_input "Empty input")
  | init :: rest ->
      let width = Array.length init in
      let initial_positions =
        Array.map (fun c -> Util.int_of_bool (c == 'S')) init
      in
      let final_ways =
        List.fold_left
          (fun old_pos row ->
            let new_pos = Array.make width 0 in
            let current_row_split_count = ref 0 in

            for i = 0 to width - 1 do
              let old_ways = old_pos.(i) in
              if old_ways > 0 then
                match row.(i) with
                | '.' -> new_pos.(i) <- new_pos.(i) + old_ways
                | '^' ->
                    current_row_split_count := !current_row_split_count + 1;
                    if i > 0 then new_pos.(i - 1) <- new_pos.(i - 1) + old_ways;
                    if i + 1 < width then
                      new_pos.(i + 1) <- new_pos.(i + 1) + old_ways
                | ch ->
                    raise
                      (Common.Bad_input
                         (Format.sprintf "Invalid character : %c" ch))
            done;
            new_pos)
          initial_positions rest
      in
      Array.fold_left ( + ) 0 final_ways |> string_of_int
