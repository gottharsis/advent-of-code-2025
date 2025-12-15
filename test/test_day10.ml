open Aoc2025.Day10

let test_parse_target () =
  let str = "[.##.]" in
  let expected = 0b0110 in
  Alcotest.(check int) "Parse target correctly" expected (parse_target str);

  let str = "[...#.]" in
  let expected = 0b01000 in
  Alcotest.(check int) "Parse target correctly" expected (parse_target str)

let test_parse_button () =
  let singleton = parse_button "(2)" in
  Alcotest.(check (list int)) "Parse singleton" [ 2 ] singleton;
  let double = parse_button "(2,3)" in
  Alcotest.(check (list int)) "Parse double" [ 2; 3 ] double;

  let out_of_order = parse_button "(3,2)" in
  Alcotest.(check (list int)) "Parse double" [ 3; 2 ] out_of_order

let test_parse_joltages () =
  let joltages = parse_joltages "{2,3,10}" in
  Alcotest.(check (list int)) "Parse joltages" [ 2; 3; 10 ] joltages

let test_parse_line () =
  let line = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" in
  let { target; joltages; buttons } = parse_line line in
  Alcotest.(check int) "target" target 0b0110;
  Alcotest.(check (list (list int)))
    "buttons" buttons
    [ [ 3 ]; [ 1; 3 ]; [ 2 ]; [ 2; 3 ]; [ 0; 2 ]; [ 0; 1 ] ];
  Alcotest.(check (list int)) "Joltages" [ 3; 5; 4; 7 ] joltages

let suite =
  let open Alcotest in
  let tc n f = test_case n `Quick f in
  [
    ( "Parse input",
      [
        tc "Target" test_parse_target;
        tc "Buttons" test_parse_button;
        tc "Joltages" test_parse_joltages;
        tc "Full line" test_parse_line;
      ] );
  ]
