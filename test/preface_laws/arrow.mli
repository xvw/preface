(** Automatization over arrow laws *)

open Aliases

module type LAWS = sig
  include Category.LAWS

  val law1 : string * (unit -> ('a, 'a) t pair)

  val law2 : string * (('a -> 'b) -> ('b -> 'c) -> ('a, 'c) t pair)

  val law3 : string * (('a -> 'b) -> ('a * 'c, 'b * 'c) t pair)

  val law4 : string * (('a, 'b) t -> ('b, 'c) t -> ('a * 'd, 'c * 'd) t pair)

  val law5 : string * (('a, 'b) t -> ('c -> 'd) -> ('a * 'c, 'b * 'd) t pair)

  val law6 : string * (('a, 'b) t -> ('a * 'c, 'b) t pair)

  val law7 : string * (('a, 'b) t -> (('a * 'c) * 'd, 'b * ('c * 'd)) t pair)
end

module Laws (A : Preface_specs.ARROW) : LAWS with type ('a, 'b) t = ('a, 'b) A.t
