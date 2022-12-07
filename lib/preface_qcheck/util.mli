(** Some utilities to deal with QCheck. *)

val test :
     count:int
  -> ?print:'generated QCheck2.Print.t
  -> 'generated QCheck2.Gen.t
  -> (unit -> ('b, 'c) Preface_laws.Law.t)
  -> (('b -> 'c) -> ('b -> 'c) -> 'generated -> bool)
  -> QCheck2.Test.t
(** An helper to deal with test definition*)

(** {1 Generators}

    Additional generators. *)

val gen_either :
  'a QCheck2.Gen.t -> 'b QCheck2.Gen.t -> ('a, 'b) Either.t QCheck2.Gen.t

val gen_result :
  'a QCheck2.Gen.t -> 'b QCheck2.Gen.t -> ('a, 'b) Result.t QCheck2.Gen.t

val gen_try : 'a QCheck2.Gen.t -> ('a, exn) Result.t QCheck2.Gen.t
val gen_exn : exn QCheck2.Gen.t

(** {1 Additional observers} *)

val obs_either :
     'a QCheck2.Observable.t
  -> 'b QCheck2.Observable.t
  -> ('a, 'b) Either.t QCheck2.Observable.t

val obs_result :
     'a QCheck2.Observable.t
  -> 'b QCheck2.Observable.t
  -> ('a, 'b) Result.t QCheck2.Observable.t

val obs_try : 'a QCheck2.Observable.t -> ('a, exn) Result.t QCheck2.Observable.t
val obs_exn : exn QCheck2.Observable.t

(** {1 Pretty-printers}

    Additional pretty-printer. *)

val pp_either :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) Stdlib.Either.t
  -> unit

val pp_result :
     (Format.formatter -> 'a -> unit)
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) Result.t
  -> unit

val pp_try :
     (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> ('a, exn) Result.t
  -> unit

val pp_exn : Format.formatter -> exn -> unit

(** {1 Equality}

    Additional equivalence function. *)

val equal_pair :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool

val equal_either :
     ('a -> 'a -> bool)
  -> ('b -> 'b -> bool)
  -> ('a, 'b) Either.t
  -> ('a, 'b) Either.t
  -> bool

val equal_exn : exn -> exn -> bool
