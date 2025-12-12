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
  pairs
  |> List.fold_left
       (fun acc corners -> Int.max acc @@ Util.uncurry2 area corners)
       (-1)
  |> string_of_int

(* We can check if a rectangle is fully contained within the polygon by making sure its edges don't cross any polygon edges.
  Overlapping a parallel edge is ok, but crossing a perpendicular one is not. We also assume all edges are either horizontal or vertical. *)

let get_x = fst
let get_y = snd
let is_vertical (p1, p2) = get_x p1 == get_x p2

(** [between x y z] returns true if a <= x <= b or b <= x <= a or *)
let between a b x = (a <= x && x <= b) || (b <= x && x <= a)

(** [crosses e1 e2] returns whether the line segments intersect. This assumes
    that they are grid aligned lines *)

(* returns the list of vertical edges, (x1, y1) (x2, y2) with y1 < y2  sorted by x*)
let vertical_edges points =
  let edges = Util.successive_pairs (points @ [ List.hd points ]) in
  edges |> Seq.filter is_vertical |> List.of_seq
  |> List.sort (fun e1 e2 ->
      let p1, _ = e1 and p2, _ = e2 in
      Int.compare (get_x p1) (get_x p2))

(* returns the list of vertical edges, (x1, y1) (x2, y2) with y1 < y2  sorted by x*)

(* let horizontal_edges points =
  let edges = Util.successive_pairs points in
  edges
  |> Seq.filter_map (fun (p1, p2) ->
      if not @@ is_vertical (p1, p2) then
        Some (if get_x p1 < get_x p2 then (p1, p2) else (p2, p1))
      else None)
  |> List.of_seq
  |> List.sort (fun e1 e2 ->
      let p1, _ = e1 and p2, _ = e2 in
      Int.compare (get_y p1) (get_y p2)) *)

(* get vertical spans for each x *)

(* we only need to consider points of interest (corners) not places in between *)

module CompressedSpace = struct
  module Bidi = Bidi_map.Make (Int) (Int)

  type t = { xmap : Bidi.t; ymap : Bidi.t }

  let get_x_orig x_comp m = Bidi.b_to_a_exn x_comp m.xmap
  let get_x_compressed x m = Bidi.a_to_b_exn x m.xmap
  let get_y_orig y_comp m = Bidi.b_to_a_exn y_comp m.ymap
  let get_y_compressed y m = Bidi.a_to_b_exn y m.ymap
  let compress_point (x, y) m = (get_x_compressed x m, get_y_compressed y m)
  let orig_point (x, y) m = (get_x_orig x m, get_y_orig y m)

  let compress_points points =
    let all_x =
      points |> Iter.of_list |> Iter.map get_x |> Iter.to_list
      |> List.sort Int.compare
    in
    let all_y =
      points |> Iter.of_list |> Iter.map get_y |> Iter.to_list
      |> List.sort Int.compare
    in
    let xmap =
      Iter.of_list all_x
      |> Iter.foldi (fun acc x i -> Bidi.add (i, x) acc) Bidi.empty
    in
    let ymap =
      Iter.of_list all_y
      |> Iter.foldi (fun acc y i -> Bidi.add (i, y) acc) Bidi.empty
    in
    let map = { xmap; ymap } in
    let transformed_points =
      points
      |> List.map (fun (x, y) ->
          (get_x_compressed x map, get_y_compressed y map))
    in
    (map, transformed_points)
end

let part2 lines =
  let points = List.map parse_point lines in
  let cmap, compressed_points = CompressedSpace.compress_points points in
  print_endline "Compressed points";
  let max_x, max_y =
    List.fold_left
      (fun (bx, by) (cx, cy) -> (Int.max bx cx, Int.max by cy))
      (-1, -1) compressed_points
  in
  Format.printf "Dimensions (Compressed): %d %d@." max_x max_y;
  let vert_edge_grid = Matrix.create (max_x + 1) (max_y + 1) 0 in
  (* let print_grid grid =
    for j = 0 to max_y do
      for i = 0 to max_x do
        Format.printf "%s" (if Matrix.get grid (i, j) == 1 then "X" else ".")
      done;
      Format.printf "@."
    done
  in *)
  (* populate all vertical edges *)
  Format.print_string "Creating vertical edges\n";
  let vertical_edges = vertical_edges compressed_points in
  vertical_edges
  |> List.iter (fun ((x1, y1), (x2, y2)) ->
      assert (x1 == x2);
      Format.printf "Processing (%d, %d) -> (%d, %d)@." x1 y1 x2 y2;
      let ymin, ymax, v = if y1 < y2 then (y1, y2, 1) else (y2, y1, -1) in
      Matrix.fill (x1, ymin) (x1 + 1, ymax + 1) v vert_edge_grid
      (* print_grid vert_edge_grid *));

  let canvas = Matrix.create (max_x + 1) (max_y + 1) 0 in

  (* fill in between them *)
  for j = 0 to max_y do
    let inside = ref 0 in
    for i = 0 to max_x do
      let v = Matrix.get vert_edge_grid (i, j) in
      if !inside <> v then inside := !inside + v;
      if !inside <> 0 || v <> 0 then Matrix.set canvas (i, j) 1
    done
  done;
  (* print_endline "final grid: ";
  print_grid canvas; *)
  (* now grid is populated *)
  let dp = Matrix.create (max_x + 2) (max_y + 2) 0 in
  let open Matrix in
  for i = 1 to max_x + 1 do
    for j = 1 to max_y + 1 do
      let left = get dp (i - 1, j) in
      let above = get dp (i, j - 1) in
      let double_count = get dp (i - 1, j - 1) in
      let current = get canvas (i - 1, j - 1) in
      let value = left + above - double_count + current in
      set dp (i, j) value
    done
  done;

  let is_valid topleft botright =
    let area = area topleft botright in
    let x1, y1 = topleft and x2, y2 = botright in
    let filled =
      Matrix.(
        get dp (x2 + 1, y2 + 1)
        - get dp (x1, y2 + 1)
        - get dp (x2 + 1, y1)
        + get dp (x1, y1))
    in
    area == filled
  in

  let pairs = Util.pairs compressed_points in
  let best =
    Iter.(
      pairs |> of_list
      |> map (fun ((x1, y1), (x2, y2)) ->
          ((Int.min x1 x2, Int.min y1 y2), (Int.max x1 x2, Int.max y1 y2)))
        (* topleft botright*)
      |> fold
           (fun acc (topleft, botright) ->
             if is_valid topleft botright then
               Int.max acc
                 (area
                    (CompressedSpace.orig_point topleft cmap)
                    (CompressedSpace.orig_point botright cmap))
             else acc)
           0)
  in
  string_of_int best
