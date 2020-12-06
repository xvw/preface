(** Extension of QCheck.Gen with additional generators. *)

(** {2 Types} *)

type 'a t = Random.State.t -> 'a
(** A Random generator for type ['a]. *)

val identity : 'a t -> 'a Preface_stdlib.Identity.t t
(** Generator for [Identity]. *)

val either :
  ?distribution:float -> 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Either.t t
(** Generator for [Either.t]. *)

val nonempty_list_size : int t -> 'a t -> 'a Preface_core.Nonempty_list.t t
(** Generator for [Nonempty_list.t] with a size generator and an element
    generator. *)

val nonempty_list : 'a t -> 'a Preface_core.Nonempty_list.t t
(** Generator for [Nonempty_list] using [nat] as sized-bound. *)

val small_nonempty_list : 'a t -> 'a Preface_core.Nonempty_list.t t
(** Generator for [Nonempty_list] using [small_nat] as sized-bound. *)

val continuation : 'a t -> 'a Preface_stdlib.Continuation.t t
(** Generator for [Continuation]. *)

val exn : exn t
(** Generator for [exception]. *)

val result : ?distribution:float -> 'a t -> 'b t -> ('a, 'b) result t
(** Generator for [result]. *)

val validation :
  ?distribution:float -> 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Validation.t t
(** Generator for [Validation.t]. *)

val try_ : ?distribution:float -> 'a t -> 'a Preface_stdlib.Try.t t
(** Generator for [Try.t]. *)

val validate : ?distribution:float -> 'a t -> 'a Preface_stdlib.Validate.t t
(** Generator for [Validate.t]. *)

val stream : 'a t -> 'a Preface_stdlib.Stream.t t
(** Generator for [Stream.t]. *)

val state : ('a -> 'b) -> 'a -> 'c -> 'b * 'c
(** Generator for [State.t].*)

(** {2 QCheck Gen API} *)

include module type of QCheck.Gen with type 'a t := 'a t
