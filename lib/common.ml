module type Day = sig
  val part1 : string list -> string
  val part2 : string list -> string

end
exception Bad_input of string

let run_day (module D : Day) (day:int)  (test: bool) =
  let filename = Input.file_path day test in
  let lines = Input.read_lines filename in
  let ans1 = try D.part1 lines with Bad_input s -> "Failed: " ^ s in
  let ans2 = try D.part2 lines with Bad_input s -> "Failed: " ^ s in

  Printf.printf "Day %d (%s)\n" day (if test then "test" else "real");
  Printf.printf "  part1: %s\n" ans1;
  Printf.printf "  part2: %s\n" ans2;
  print_newline ()

