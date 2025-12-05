open Aoc2025
let usage_message = "aoc [-t] <day-number>"
let use_test_input = ref false
let day_num = ref 0

let anon_fun day =
  day_num := int_of_string day

let invalid_day day =
  Printf.eprintf "Day %d not implemented yet\n" day;
  exit 1

let get_day day =
  match day with
  | 1 -> (module Day01 : Common.Day)
  | 2 -> (module Day02 : Common.Day)
  | 3 -> (module Day03 : Common.Day)
  | x -> invalid_day x

let speclist =
  [("-t", Arg.Set use_test_input, "Use test input")]

let () =
  Arg.parse speclist anon_fun usage_message;
  let day_module = get_day !day_num in
  Common.run_day day_module !day_num !use_test_input

