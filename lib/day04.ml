let part1 lines =
  let mat = Matrix.char_matrix_of_lines lines in
  let num_neighbors point =
    Matrix.all_neighbors mat point |> CCList.count (fun (_, ch) -> ch == '@')
  in
  mat |> Matrix.to_seqi
  |> CCSeq.filter (fun (point, ch) ->
      if ch <> '@' then false
      else
        let n = num_neighbors point in
        (* let  i, j = point in Format.printf "Neighbors of %d %d: %d@." i j n; *)
        n < 4)
  |> CCSeq.length |> string_of_int

let part2 _lines = "Not implemented"
