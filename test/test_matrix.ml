open Aoc2025

let test_create_dimensions () =
  let mat = Matrix.create 2 3 1 in
  Alcotest.(check int) "n_rows" 2 (Matrix.rows mat)

let test_create_value () =
  let mat = Matrix.create 2 3 1 in
  let value_seq = Matrix.to_seqi mat |> Seq.map snd in
  Alcotest.(check (seq int))
    "Correct values"
    (List.to_seq [ 1; 1; 1; 1; 1; 1 ])
    value_seq

let test_create_bad_grid () =
  let jagged =
    Array.of_list [ Array.of_list [ 1; 2 ]; Array.of_list [ 1; 2; 3 ] ]
  in
  Alcotest.(check_raises)
    "Raises error"
    (Common.Bad_input "Incorrect row length at row 1: got 3 expected 2")
    (fun () ->
      let _ = Matrix.matrix_of_grid jagged in
      ())

let test_neighbors_3_3_helper pos expected =
  let grid = Matrix.create 3 3 1 in
  let neighbors = Matrix.all_neighbors grid pos |> List.map fst in
  Alcotest.(check (list (pair int int)))
    "All positions present in correct order" expected neighbors

let test_neighbors_3_3_values () =
  let grid =
    Matrix.matrix_of_2d_list [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
  in
  let neighbors = Matrix.all_neighbors grid (1, 1) in
  let expected =
    [
      ((0, 1), 2);
      ((0, 2), 3);
      ((1, 2), 6);
      ((2, 2), 9);
      ((2, 1), 8);
      ((2, 0), 7);
      ((1, 0), 4);
      ((0, 0), 1);
    ]
  in
  Alcotest.(check (list (pair (pair int int) int)))
    "All positions present in correct order" expected neighbors

let test_neighbors_center () =
  test_neighbors_3_3_helper (1, 1)
    [ (0, 1); (0, 2); (1, 2); (2, 2); (2, 1); (2, 0); (1, 0); (0, 0) ]

let test_neighbors_topleft () =
  test_neighbors_3_3_helper (0, 0) [ (0, 1); (1, 1); (1, 0) ]

let test_neighbors_botright () =
  test_neighbors_3_3_helper (2, 2) [ (1, 2); (2, 1); (1, 1) ]

let test_seqi () =
  let grid = Matrix.matrix_of_2d_list [ [ 1; 2 ]; [ 3; 4 ] ] in
  let seq = Matrix.to_seqi grid
  and expected =
    List.to_seq [ ((0, 0), 1); ((0, 1), 2); ((1, 0), 3); ((1, 1), 4) ]
  in
  Alcotest.(check (seq (pair (pair int int) int)))
    "Iterates correctly" expected seq

let suite =
  let open Alcotest in
  let tc n f = test_case n `Quick f in
  [
    ( "Create",
      [
        tc "Dimensions" test_create_dimensions;
        tc "Values" test_create_value;
        tc "Jagged array raises exception" test_create_bad_grid;
      ] );
    ( "Neighbors",
      [
        tc "Center" test_neighbors_center;
        tc "Top left Corner" test_neighbors_topleft;
        tc "Bottom Right Corner" test_neighbors_botright;
        tc "Values" test_neighbors_3_3_values;
      ] );
    ("Seq Iter", [ tc "Seqi" test_seqi ]);
  ]
