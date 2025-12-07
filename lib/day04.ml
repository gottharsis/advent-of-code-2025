let part1 lines =
  let mat = Matrix.char_matrix_of_lines lines in
  mat |> Matrix.to_seqi
  |> Seq.map (fun (point, _) ->
      let num_neighbors =
        Matrix.all_neighbors mat point
        |> List.fold_left
             (fun acc (_, ch) -> if ch == '@' then acc + 1 else acc)
             0
      in
      if num_neighbors < 4 then 1 else 0)
  |> Seq.fold_left ( + ) 0 |> string_of_int

let part2 _lines = "Not implemented"
