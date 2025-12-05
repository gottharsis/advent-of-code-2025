open Containers

let joltages line = line 
|> String.to_seq |> List.of_seq
|> List.map (fun (a) -> Char.code a - Char.code '0')

let max_joltage joltages = 
    let rec aux current_left current_right lst = 
        match lst with 
        | (d1 :: d2 :: rest) -> 
                if d1 > current_left && d2 > current_right  then aux d1 d2 (d2 :: rest)
                else if d1 > current_left then aux d1 d2 (d2 :: rest) 
                else if d2 > current_right then aux current_left d2 (d2 :: rest)
                else aux current_left current_right (d2 :: rest)
        | _ -> (current_left, current_right)
    in 
    let (left, right) = aux 0 0 joltages in
    10 * left + right



let part1 lines = lines |> List.map joltages |> List.map max_joltage |> List.fold_left (+) 0 |> string_of_int

(* returns idx, max_elem *)
let find_max_in_slice array start len = 
    array |> CCArray.to_seq |> CCSeq.drop start |> CCSeq.take len 
        |> CCSeq.fold_lefti (fun (max_elemi, max_elem) i elem -> if elem > max_elem then (i, elem) else (max_elemi, max_elem)) (-1, -1)
        


let max_joltages2 joltages = 
    let joltages = Array.of_list joltages in
    let n = Array.length joltages in
        


let part2 lines =lines |> List.map joltages |> List.map max_joltages2 |> List.fold_left (+) 0 |> string_of_int 
