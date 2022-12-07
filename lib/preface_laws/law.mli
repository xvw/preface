type ('a, 'b) t
type ('a, 'b) side

val pp : Format.formatter -> ('a, 'b) t -> unit
val law : ('a, 'b) side -> ('a, 'b) side -> ('a, 'b) t
val ( =~ ) : string -> ('a -> 'b) -> ('a, 'b) side
val name : ('a, 'b) t -> string
val get_sides : ('a, 'b) t -> ('a -> 'b) * ('a -> 'b)
