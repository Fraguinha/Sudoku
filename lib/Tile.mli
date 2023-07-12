type t
type pos = int * int * int

val pos : t -> pos
val value : t -> int
val set : t -> int -> t
val make : pos -> int -> bool -> t
