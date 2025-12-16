let parse_problem line =
  let%match [ dims; counts ] = CCString.split ~by:": " line in
  let%match [ a; b ] = CCString.split ~by:"x" dims |> List.map int_of_string in
  let cnts = String.split_on_char ' ' counts |> CCList.map int_of_string in

  ((a, b), cnts)

let part1 lines =
  let open CCFun.Infix in
  let shape_area =
    CCSeq.(
      of_list %> flat_map String.to_seq
      %> fold_left (fun acc i -> acc + Bool.to_int (Char.equal i '#')) 0)
  in
  let%match [ s0; s1; s2; s3; s4; s5; problems ] =
    let eq = CCFun.compose_binop CCString.is_empty Bool.equal in
    lines |> Iter.of_list |> Iter.group_succ_by ~eq
    |> Iter.filter_mapi (fun idx x -> if idx mod 2 == 0 then Some x else None)
    |> Iter.to_list
  in
  let areas = Array.map shape_area [| s0; s1; s2; s3; s4; s5 |] in

  let solve_problem ((x, y), cnts) =
    let required =
      Iter.(of_list cnts |> mapi (fun i n -> n * areas.(i)) |> sum)
    in
    let available = x * y in
    required <= available
  in
  problems
  |> Iter.(of_list %> map parse_problem %> filter solve_problem %> length)
  |> string_of_int

let part2 _ = ""
