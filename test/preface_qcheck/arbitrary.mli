(** Extension of QCheck with additional arbitraries. *)

(** {2 Types} *)

type 'a t = 'a QCheck.arbitrary
(** A value of type ['a t] is an object with a method for generating random
    values of type ['a], and additional methods to compute the size of values,
    print them, and possibly shrink them into smaller counter-examples.*)

(** {2 Additional Arbitraries} *)

val identity :
     ?collect:('a Preface_stdlib.Identity.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Identity.t t
(** Arbitrary for [Identity.t] *)

val exn :
  ?collect:(Preface_stdlib.Exn.t -> string) -> unit -> Preface_stdlib.Exn.t t
(** Arbitrary for [Exn.t] *)

val either :
     ?collect:(('a, 'b) Preface_stdlib.Either.t -> string)
  -> 'a t
  -> 'b t
  -> ('a, 'b) Preface_stdlib.Either.t t
(** Arbitrary for [Either.t] *)

val result :
     ?collect:(('a, 'b) Preface_stdlib.Result.t -> string)
  -> 'a t
  -> 'b t
  -> ('a, 'b) Preface_stdlib.Result.t t
(** Arbitrary for [Result.t] *)

val try_ :
     ?collect:('a Preface_stdlib.Try.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Try.t t
(** Arbitrary for [Try.t] *)

val validation :
     ?collect:(('a, 'b) Preface_stdlib.Validation.t -> string)
  -> 'a t
  -> 'b t
  -> ('a, 'b) Preface_stdlib.Validation.t t
(** Arbitrary for [Validation.t] *)

val validate :
     ?collect:('a Preface_stdlib.Validate.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Validate.t t
(** Arbitrary for [Validate.t] *)

val nonempty_list :
     ?collect:('a Preface_stdlib.Nonempty_list.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Nonempty_list.t t
(** Arbitrary for [Nonempty_list.t] *)

val continuation :
     ?collect:('a Preface_stdlib.Continuation.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Continuation.t t
(** Arbitrary for [Continuation.t] *)

val stream :
     ?collect:('a Preface_stdlib.Stream.t -> string)
  -> 'a t
  -> 'a Preface_stdlib.Stream.t t
(** Arbitrary for [Stream.t] *)

(** {2 QCheck's arbitraries} *)

val unit : unit t

val bool : bool t

val float : float t

val pos_float : float t

val neg_float : float t

val float_bound_inclusive : float -> float t

val float_bound_exclusive : float -> float t

val float_range : float -> float -> float t

val int : int t

val int_bound : int -> int t

val int_range : int -> int -> int t

val small_nat : int t

val small_signed_int : int t

val int32 : int32 t

val int64 : int64 t

val pos_int : int t

val small_int_corners : unit -> int t

val neg_int : int t

val char : char t

val printable_char : char t

val numeral_char : char t

val string_gen_of_size : int Gen.t -> char Gen.t -> string t

val string_gen : char Gen.t -> string t

val string : string t

val small_string : string t

val small_list : 'a t -> 'a list t

val string_of_size : int Gen.t -> string t

val printable_string : string t

val printable_string_of_size : int Gen.t -> string t

val small_printable_string : string t

val numeral_string : string t

val numeral_string_of_size : int Gen.t -> string t

val list : 'a t -> 'a list t

val list_of_size : int Gen.t -> 'a t -> 'a list t

val array : 'a t -> 'a array t

val array_of_size : int Gen.t -> 'a t -> 'a array t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val option : 'a t -> 'a option t
