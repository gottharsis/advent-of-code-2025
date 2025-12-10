open Aoc2025

let test_pairs_array () =
  let array = [| 1; 2; 3 |] in
  let expected = [ (1, 2); (1, 3); (2, 3) ] in
  let actual = Util.array_pairs array in
  Alcotest.(check (list (pair int int))) "correct pairs" expected actual

let test_pairs_list () =
  let array = [ 1; 2; 3 ] in
  let expected = [ (1, 2); (1, 3); (2, 3) ] in
  let actual = Util.pairs array in
  Alcotest.(check (list (pair int int))) "correct pairs" expected actual

let suite =
  let open Alcotest in
  let tc n f = test_case n `Quick f in
  [
    ( "Util::array_pairs",
      [ tc "Array pairs" test_pairs_array; tc "List pairs" test_pairs_list ] );
  ]
