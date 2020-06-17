(** Extension of QCheck.Shrink with additional shrinker *)

(** {2 Types} *)

type 'a t = 'a QCheck.Shrink.t

(** {2 Additional Shrinkers} *)

val identity : 'a t -> 'a Preface_stdlib.Identity.t t
(** Shrinker for [Identity.t]. *)

(** {2 QCheck Shrink API} *)

include module type of QCheck.Shrink with type 'a t := 'a t
