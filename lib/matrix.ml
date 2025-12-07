type 'a t = { data : 'a array array; n_rows : int; n_cols : int }

let rows mat = mat.n_rows
let cols mat = mat.n_cols

let create rows cols value =
  { data = Array.make_matrix rows cols value; n_rows = rows; n_cols = cols }

let matrix_of_grid grid =
  let n_rows = Array.length grid in
  if n_rows == 0 then raise (Common.Bad_input "Empty grid");
  let n_cols = Array.length grid.(0) in
  for i = 1 to n_rows - 1 do
    if Array.length grid.(i) <> n_cols then
      raise
        (Common.Bad_input
           (Format.sprintf "Incorrect row length at row %d: got %d expected %d"
              i
              (Array.length grid.(i))
              n_cols))
  done;
  { data = grid; n_rows; n_cols }

let matrix_of_2d_list lst =
  match lst with
  | [] -> raise (Common.Bad_input "Empty list")
  | first_row :: _ -> (
      match first_row with
      | [] -> raise (Common.Bad_input "Empty col")
      | value :: _ ->
          let n_rows = List.length lst and n_cols = List.length first_row in
          let data = Array.make_matrix n_rows n_cols value in
          lst
          |> List.iteri (fun i row ->
              row |> List.iteri (fun j value -> data.(i).(j) <- value));
          { data; n_rows; n_cols })

let char_matrix_of_lines (lines : string list) =
  match lines with
  | [] -> raise (Common.Bad_input "Cannot create grid of empty strings")
  | first_line :: _ ->
      let n_rows = List.length lines and n_cols = String.length first_line in
      let grid = Array.make_matrix n_rows n_cols 'a' in

      lines
      |> List.iteri (fun i line ->
          if String.length line <> n_cols then
            raise (Common.Bad_input "All lines must have equsl length");
          line |> String.iteri (fun j c -> grid.(i).(j) <- c));

      { data = grid; n_rows; n_cols }

let get_row mat i = mat.data.(i)

let get mat point =
  let i, j = point in
  mat.data.(i).(j)

let set mat point v =
  let i, j = point in
  mat.data.(i).(j) <- v

let apply_delta point delta =
  let i, j = point and di, dj = delta in
  (i + di, j + dj)

let is_valid_index mat point =
  let i, j = point in
  i >= 0 && i < mat.n_rows && j >= 0 && j < mat.n_cols

let n = (-1, 0)
let s = (1, 0)
let e = (0, 1)
let w = (0, -1)
let ne = (-1, 1)
let nw = (-1, -1)
let se = (1, 1)
let sw = (1, -1)

let neighbors_h (directions : (int * int) list) (mat : 'a t) (point : int * int)
    =
  directions
  |> List.filter_map (fun delta ->
      let new_pt = apply_delta point delta in
      if is_valid_index mat new_pt then
        let i, j = new_pt in
        let item = ((i, j), mat.data.(i).(j)) in
        Some item
      else None)

let all_neighbors mat point =
  neighbors_h [ n; ne; e; se; s; sw; w; nw ] mat point

let cardinal_neighbors mat point = neighbors_h [ n; e; s; w ] mat point

let map f mat =
  let new_data = Array.map (Array.map f) mat.data in
  { mat with data = new_data }

let iter f mat = Array.iter (Array.iter f) mat.data

let iteri f mat =
  let row_iter i row = Array.iteri (fun j value -> f (i, j) value) row in
  Array.iteri row_iter mat.data

let to_seqi mat =
  let visit (i, j) =
    (* assume j is always valid*)
    if i >= mat.n_rows then None
    else
      let item = ((i, j), mat.data.(i).(j))
      and next_state = if j + 1 >= mat.n_cols then (i + 1, 0) else (i, j + 1) in
      Some (item, next_state)
  in
  Seq.unfold visit (0, 0)
