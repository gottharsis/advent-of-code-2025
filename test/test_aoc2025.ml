open Alcotest

let () =
  run "AOC Helper Tests" @@ List.concat [ Test_matrix.suite; Test_util.suite ]
