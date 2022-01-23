open Aliases

module type LAWS = sig
  include Arrow.LAWS

  val law8 : string * (unit -> ('a * 'b, 'a * 'b) t pair)
  val law9 : string * (('a, 'b) t -> (('b, 'c) t * 'a, 'c) t pair)
  val law10 : string * (('a, 'b) t -> (('c, 'a) t * 'c, 'b) t pair)
end

module Laws (A : Preface_specs.ARROW_APPLY) :
  LAWS with type ('a, 'b) t := ('a, 'b) A.t
