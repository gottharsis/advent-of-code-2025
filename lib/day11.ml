module StringLookup = CCBijection.Make (String) (Int)

let parse_input lines =
  let lkp = ref StringLookup.empty in
  let latest_unused = ref 0 in
  let get_int str =
    if StringLookup.mem_left str !lkp then StringLookup.find_left str !lkp
    else
      let r = !latest_unused in
      let () = incr latest_unused in
      let () = lkp := StringLookup.add str r !lkp in
      r
  in

  let adj =
    Iter.of_list lines
    |> Iter.map (fun line ->
        match String.split_on_char ' ' line with
        | source :: rest ->
            let src =
              String.sub source 0 (String.length source - 1)
              (* remove colon *)
            in
            (src, rest)
        | [] -> raise (Common.Bad_input ("Invalid edge " ^ line)))
    |> Iter.map (fun (source, rest) -> (get_int source, List.map get_int rest))
    |> Iter.fold
         (fun acc (source, rest) -> CCIntMap.add source rest acc)
         CCIntMap.empty
  in
  (!lkp, adj)

let topsort n adj =
  let indegree = Array.make n 0 in
  let () =
    CCIntMap.iter
      (fun _u vs -> List.iter (fun v -> indegree.(v) <- indegree.(v) + 1) vs)
      adj
  in
  let queue = Queue.create () in
  let () =
    Array.iteri (fun u indeg -> if indeg == 0 then Queue.add u queue) indegree
  in
  let rec seq () =
    if Queue.is_empty queue then Seq.Nil
    else
      let curr = Queue.pop queue in
      let () =
        CCIntMap.find curr adj |> Option.value ~default:[]
        |> List.iter (fun v ->
            indegree.(v) <- indegree.(v) - 1;
            if indegree.(v) == 0 then Queue.add v queue)
      in
      Seq.Cons (curr, seq)
  in
  seq

let num_paths topsort n adj src dest =
  let ways = Array.make n 0 in
  let () = ways.(src) <- 1 in
  let () =
    topsort
    |> Seq.iter (fun u ->
        let vs = CCIntMap.find u adj |> Option.value ~default:[] in
        vs |> List.iter (fun v -> ways.(v) <- ways.(v) + ways.(u)))
  in
  ways.(dest)

let part1 lines =
  let lkp, adj = parse_input lines in
  let n = StringLookup.to_list lkp |> List.length in
  let topsort = topsort n adj in
  let ways =
    StringLookup.(
      num_paths topsort n adj (find_left "you" lkp) (find_left "out" lkp))
  in
  string_of_int ways

let part2 lines =
  let lkp, adj = parse_input lines in
  let n = StringLookup.to_list lkp |> List.length in
  let topsort = topsort n adj |> CCSeq.memoize in
  let np src dest =
    StringLookup.(
      num_paths topsort n adj (find_left src lkp) (find_left dest lkp))
  in
  (* svr -> dac -> fft -> out *)
  let p1 = np "svr" "dac" * np "dac" "fft" * np "fft" "out" in
  let p2 = np "svr" "fft" * np "fft" "dac" * np "dac" "out" in

  string_of_int (p1 + p2)
