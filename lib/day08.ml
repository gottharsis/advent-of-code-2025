type point3 = int * int * int

let string_of_p3 (a, b, c) = Format.sprintf "(%d, %d, %d)" a b c

let string_of_edge (e1, e2) =
  Format.sprintf "%s -> %s" (string_of_p3 e1) (string_of_p3 e2)

let read_line line =
  line |> String.split_on_char ',' |> List.map int_of_string |> fun x ->
  match x with
  | [ a; b; c ] -> (a, b, c)
  | _ -> raise (Common.Bad_input "Invalid point definition")

let dist2 (x1, y1, z1) (x2, y2, z2) =
  let d1 = x2 - x1 and d2 = y2 - y1 and d3 = z2 - z1 in
  (d1 * d1) + (d2 * d2) + (d3 * d3)

let part1 lines =
  let points = List.map read_line lines |> Array.of_list in
  let n = Array.length points in
  let edges =
    CCList.range' 0 n
    |> CCList.flat_map (fun u ->
        CCList.range' 0 u |> CCList.map (fun v -> (u, v)))
  in
  let point_dist i1 i2 = dist2 points.(i1) points.(i2) in
  let edge_len (u, v) = point_dist u v in
  let module ShortEdgeHeap = CCHeap.Make_from_compare (struct
    type t = int * int

    let compare e1 e2 = Int.compare (edge_len e1) (edge_len e2)
  end) in
  let heap = ShortEdgeHeap.of_list edges in
  let uf = Union_find.create n in
  heap |> ShortEdgeHeap.to_seq_sorted |> Seq.take 1000
  |> Seq.iter (fun (u, v) ->
      (* print_endline ("Combining " ^ string_of_edge (points.(u), points.(v))); *)
      Union_find.union uf u v);
  let component_sizes =
    Union_find.components uf |> List.map List.length |> List.sort Int.compare
    |> List.rev
  in
  match component_sizes with
  | a :: b :: c :: _ -> string_of_int (a * b * c)
  | _ -> raise (Common.Bad_input "Not enough components")

let part2 lines =
  let points = List.map read_line lines |> Array.of_list in
  let n = Array.length points in
  let edges =
    CCList.range' 0 n
    |> CCList.flat_map (fun u ->
        CCList.range' 0 u |> CCList.map (fun v -> (u, v)))
  in
  let point_dist i1 i2 = dist2 points.(i1) points.(i2) in
  let edge_len (u, v) = point_dist u v in
  let module ShortEdgeHeap = CCHeap.Make_from_compare (struct
    type t = int * int

    let compare e1 e2 = Int.compare (edge_len e1) (edge_len e2)
  end) in
  let heap = ShortEdgeHeap.of_list edges in
  let uf = Union_find.create n in
  let rec loop last_edge heap =
    if Union_find.n_components uf <= 1 then last_edge
    else
      let heap2, (u, v) = ShortEdgeHeap.take_exn heap in
      Union_find.union uf u v;
      loop (u, v) heap2
  in
  let p1, p2 = loop (0, 0) heap in
  let x1, _, _ = points.(p1) and x2, _, _ = points.(p2) in
  string_of_int (x1 * x2)
