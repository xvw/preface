(** Extension of QCheck.Print with additional printer *)

(** {2 Types} *)

type 'a t = 'a QCheck.Print.t

(** {2 Additional Printers} *)

val identity : 'a t -> 'a Preface_stdlib.Identity.t t
(** Printer for [Identity.t]. *)

(** {2 QCheck Print API} *)

include module type of QCheck.Print with type 'a t := 'a t
