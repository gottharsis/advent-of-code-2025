type t = { parent : int array; size : int array }

let create size =
  { parent = Array.init size CCFun.id; size = Array.make size 0 }

let find uf x =
  let x = ref x in
  while uf.parent.(!x) <> !x do
    uf.parent.(!x) <- uf.parent.(uf.parent.(!x));
    (* Path compression *)
    x := uf.parent.(!x)
  done;
  uf.parent.(!x)

let union uf x y =
  let xroot = find uf x and yroot = find uf y in
  if xroot <> yroot then
    if uf.size.(xroot) < uf.size.(yroot) then (
      uf.parent.(xroot) <- yroot;
      uf.size.(yroot) <- uf.size.(yroot) + uf.size.(xroot))
    else (
      uf.parent.(yroot) <- xroot;
      uf.size.(xroot) <- uf.size.(xroot) + uf.size.(yroot))

let connected uf x y = find uf x = find uf y

let get_component uf x =
  let parent = find uf x and num_elements = Array.length uf.parent in
  CCList.range' 0 num_elements |> CCList.filter (fun i -> find uf i == parent)

module IntMap = Map.Make (Int)

let components uf =
  let num_elements = Array.length uf.parent in
  CCSeq.range 0 (num_elements - 1)
  |> Seq.map (fun item ->
      let parent = find uf item in
      (parent, item))
  |> Seq.fold_left
       (fun map (key, item) ->
         map
         |> IntMap.update key (fun list ->
             match list with
             | None -> Some [ item ]
             | Some items -> Some (item :: items)))
       IntMap.empty
  |> IntMap.to_list |> List.map snd
