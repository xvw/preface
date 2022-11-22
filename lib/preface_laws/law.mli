type ('a, 'b) t
type ('a, 'b) side

val pp : Format.formatter -> ('a, 'b) t -> unit
val law : lhs:('a, 'b) side -> rhs:('a, 'b) side -> ('a, 'b) t
val ( =~ ) : string -> ('a -> 'b) -> ('a, 'b) side
