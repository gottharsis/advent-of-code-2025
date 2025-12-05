let usage_message = "aoc [-t] <day-number>"
let use_test_input = ref false
let day_num = ref 0

let anon_fun day =
  day_num := int_of_string day

let invalid_day day =
  Printf.eprintf "Day %d not implemented yet\n" day;
  exit 1

let speclist =
  [("-t", Arg.Set use_test_input, "Use test input")]

let () =
  Arg.parse speclist anon_fun usage_message;
  match !day_num with
  | x -> invalid_day x

