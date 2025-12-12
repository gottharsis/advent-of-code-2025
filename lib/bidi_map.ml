module Make (A : Map.OrderedType) (B : Map.OrderedType) = struct
  module AMap = Map.Make (A)
  module BMap = Map.Make (B)

  type elt_a = A.t
  type elt_b = B.t
  type t = { a_to_b : elt_b AMap.t; b_to_a : elt_a BMap.t }

  let a_to_b_exn key map = AMap.find key map.a_to_b
  let b_to_a_exn key map = BMap.find key map.b_to_a

  let add (a, b) map =
    { a_to_b = AMap.add a b map.a_to_b; b_to_a = BMap.add b a map.b_to_a }

  let empty = { a_to_b = AMap.empty; b_to_a = BMap.empty }
end
