type t

val create : int -> t
(** [create] makes a union find data structure with the specified number of
    items *)

val union : t -> int -> int -> unit
(** [union x y] combines the components of x and y *)

val connected : t -> int -> int -> bool
(** [connected x y] returns whether x and y are in the same component *)

val get_component : t -> int -> int list
(** [get_component item] returns all integers in the same component as item *)

val components : t -> int list list
(** [components uf] returns a list of all components as lists of integers. *)
