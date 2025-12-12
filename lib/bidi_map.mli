module Make (A : Map.OrderedType) (B : Map.OrderedType) : sig
  type t
  type elt_a = A.t
  type elt_b = B.t

  val a_to_b_exn : elt_a -> t -> elt_b
  val b_to_a_exn : elt_b -> t -> elt_a
  val add : elt_a * elt_b -> t -> t
  val empty : t
end
