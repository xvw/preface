(** Exposes [Stream.t], a kind of inifinite lists

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}    
    - {!val:Comonad}
*)

(** {1 Type} *)
type 'a t

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

module Comonad : Preface_specs.COMONAD with type 'a t = 'a t
(** {2 Comonad API} *)

(** {1 Helpers} *)

val pure : 'a -> 'a t
(** Create a value from ['a] to ['a t]. *)
                                                
val stream : 'a -> 'a t Lazy.t -> 'a t
(** Build a stream manually. *)

val hd : 'a t -> 'a
(** Get the head of the stream. *)

val tl : 'a t -> 'a t
(** Get the tail of the stream. *)
