let num_neighbors mat point =
  Matrix.all_neighbors mat point |> CCList.count (fun (_, ch) -> ch == '@')

let part1 lines =
  let mat = Matrix.char_matrix_of_lines lines in
  let num_neighbors = num_neighbors mat in
  mat |> Matrix.to_seqi
  |> CCSeq.filter (fun (point, ch) ->
      if ch <> '@' then false
      else
        let n = num_neighbors point in
        (* let  i, j = point in Format.printf "Neighbors of %d %d: %d@." i j n; *)
        n < 4)
  |> CCSeq.length |> string_of_int

let part2 lines =
  let open Matrix in
  let mat = char_matrix_of_lines lines in
  let num_neighbors = num_neighbors mat in
  let is_removable point = get mat point == '@' && num_neighbors point < 4 in
  let replace_all_removable () =
    let removable_points =
      to_seqi mat
      |> Seq.filter_map (fun (pos, _) ->
          if is_removable pos then Some pos else None)
      |> List.of_seq
    in
    if List.is_empty removable_points then 0
    else (
      List.iter
        (fun point ->
          (* let i, j = point in
          Format.printf "Removing point %d %d @." i j; *)
          set mat point 'x')
        removable_points;
      List.length removable_points)
  in
  let do_removal = ref true and total_removed = ref 0 in
  while !do_removal do
    let removed_cnt = replace_all_removable () in
    do_removal := removed_cnt <> 0;
    total_removed := !total_removed + removed_cnt
  done;
  string_of_int !total_removed
