type 'a t

val create : int -> int -> 'a -> 'a t
val rows : 'a t -> int
val cols : 'a t -> int

val matrix_of_grid : 'a array array -> 'a t
(** Creates a matrix from a 2d array. Raises [Common.Bad_input] if dimensions
    are wrong *)

val matrix_of_2d_list : 'a list list -> 'a t
(** Creates a matrix from a 2d list. Raises [Common.Bad_input] if dimensions are
    wrong *)

val char_matrix_of_lines : string list -> char t
(** [char_matrix_of_lines lines] creates a matrix of chars from a list of
    equal-length strings. Raises [Common.Bad_input] if [lines] is empty or the
    lines have different lengths. *)

val get_row : 'a t -> int -> 'a array
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit

val apply_delta : int * int -> int * int -> int * int
(** [apply_delta] i j di dj = (i + di), (j + dj) *)

val is_valid_index : 'a t -> int * int -> bool
val n : int * int
val s : int * int
val e : int * int
val w : int * int
val ne : int * int
val nw : int * int
val se : int * int
val sw : int * int

val all_neighbors : 'a t -> int * int -> ((int * int) * 'a) list
(** [all_neighbors matrix point] returns a list of (i, j, value) for all *valid*
    8 direction neighbors of `point`, starting from N and continuing clockwise
*)

val cardinal_neighbors : 'a t -> int * int -> ((int * int) * 'a) list
(** [all_neighbors matrix point] returns a list of (i, j, value) for all *valid*
    NESW neighbors of `point`, starting from N and continuing clockwise *)

val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int * int -> 'a -> unit) -> 'a t -> unit
val to_seqi : 'a t -> ((int * int) * 'a) Seq.t
