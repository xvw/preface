(** Observable for generating function

    inclusion of QCheck API *)

type 'a t = 'a QCheck.Observable.t

include module type of QCheck.Observable with type 'a t := 'a t

(** {1 Custom Observable} *)

val identity : 'a t -> 'a Preface_stdlib.Identity.t t

val either : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Either.t t

val exn : exn t

val result : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Result.t t

val try_ : 'a t -> 'a Preface_stdlib.Try.t t

val stream : ?fuel:int -> 'a t -> 'a Preface_stdlib.Stream.t t

val continuation : 'a t -> 'a Preface_stdlib.Continuation.t t

val nonempty_list : 'a t -> 'a Preface_stdlib.Nonempty_list.t t

val validation : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Validation.t t

val validate : 'a t -> 'a Preface_stdlib.Validate.t t
