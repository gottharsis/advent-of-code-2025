let file_path day test =
  if test
    then Printf.sprintf "input/day%02d_test.txt" day
    else Printf.sprintf "input/day%02d.txt" day


let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
      close_in ic;
      List.rev acc
    in
    loop []
