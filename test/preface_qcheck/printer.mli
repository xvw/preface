(** Extension of QCheck.Print with additional printer *)

(** {2 Types} *)

type 'a t = 'a QCheck.Print.t

(** {2 Additional Printers} *)

val identity : 'a t -> 'a Preface_stdlib.Identity.t t
(** Printer for [Identity.t]. *)

val either : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Either.t t
(** Printer for [Either.t]. *)

val exn : Preface_stdlib.Exn.t t
(** Printer for [Exn.t]. *)

val result : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Result.t t
(** Printer for [Result.t]. *)

val validation : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Validation.t t
(** Printer for [Validation.t]. *)

val try_ : 'a t -> 'a Preface_stdlib.Try.t t
(** Printer for [Try.t]. *)

val validate : 'a t -> 'a Preface_stdlib.Validate.t t
(** Printer for [Validate.t]. *)

val nonempty_list : 'a t -> 'a Preface_stdlib.Nonempty_list.t t
(** Printer for [Nonempty_list.t]. *)

val seq : 'a t -> 'a Preface_stdlib.Seq.t t
(** Printer for [Seq.t]. *)

val stream : 'a t -> 'a Preface_stdlib.Stream.t t
(** Printer for [Stream.t]. *)

val continuation : 'a t -> 'a Preface_stdlib.Continuation.t t
(** Printer for [Continuation.t]. *)

(** {2 QCheck Print API} *)

include module type of QCheck.Print with type 'a t := 'a t
