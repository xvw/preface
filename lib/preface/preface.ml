(** {1 Standard Library} *)

module Fun = Fun

(** {2 Incarnations} *)

module Functor = Functor
module Identity = Identity

(** {1 Pervasives} *)

(** Alias for {!val:Fun.id} *)
let id x = Fun.id x

include Fun.Infix
