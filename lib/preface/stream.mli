type 'a t

val stream : 'a -> 'a t Lazy.t -> 'a t

module Comonad : Specs.COMONAD with type 'a t = 'a t
