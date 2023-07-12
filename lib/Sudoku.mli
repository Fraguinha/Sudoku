type t
type el = Tile.t

val print : t -> unit
val to_list : t -> int list
val solve : t -> t
val make : int list -> t
