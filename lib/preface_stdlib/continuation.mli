module Make (R : sig type t end) : sig
    type 'a t = ('a -> R.t) -> R.t

    val pure : 'a -> 'a t

    module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t

    module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t

    module Monad : Preface_specs.MONAD with type 'a t = 'a t
end
