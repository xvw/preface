open Aliases

module type LAWS = sig
  include Arrow.LAWS

  val law8 :
    string * (('a -> 'b) -> (('a, 'c) Either.t, ('b, 'c) Either.t) t pair)

  val law9 :
    string
    * (   ('a, 'b) t
       -> ('b, 'c) t
       -> (('a, 'd) Either.t, ('c, 'd) Either.t) t pair)

  val law10 : string * (('a, 'b) t -> ('a, ('b, 'c) Either.t) t pair)

  val law11 :
    string
    * (   ('a, 'b) t
       -> ('c -> 'd)
       -> (('a, 'c) Either.t, ('b, 'd) Either.t) t pair)

  val law12 :
    string
    * (   ('a, 'b) t
       -> ((('a, 'c) Either.t, 'd) Either.t, ('b, ('c, 'd) Either.t) Either.t) t
          pair)
end

module Laws (A : Preface_specs.ARROW_CHOICE) :
  LAWS with type ('a, 'b) t := ('a, 'b) A.t
