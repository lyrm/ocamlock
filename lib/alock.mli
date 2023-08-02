type t

val create : ?padding:int -> int -> t
(** [create ~padding capacity] creates an Array-based queue lock that
    can work with at most [capacity] domains.  [~padding] enables to
    add padding to avoid false sharing. By default, its value is 1 (no
    padding). Padding value should equal to the size a line cache
    divided by 8 (size of a word). *)

type rt

val lock : t -> rt
val lock_relax : t -> rt
val unlock : rt -> unit
