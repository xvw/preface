(** Helpers for [QCheck]. *)

(** Signature to generate connected function application. *)
module type GENERATOR = sig
  type input

  val arbitrary : input QCheck.arbitrary

  val observable : input QCheck.Observable.t
end

(** Signature to generate aribtrary over ['a t]. *)
module type REQ = sig
  type 'a t

  val suite_name : string

  val arbitrary : 'a QCheck.arbitrary -> 'a t QCheck.arbitrary
end

(** Post hook (mainly for continuation). *)
module type HOOK = sig
  type 'a t

  val apply : 'a t -> 'c
end

(** Alcotest suite *)
module type ALCOTEST_SUITE = sig
  val cases : string * unit Alcotest.test_case list
end

(** Additional Generators *)
module Gen : sig
  type 'a t = 'a QCheck.Gen.t

  val option : ?distribution:float -> 'a t -> 'a Preface_stdlib.Option.t t
  (** Generator for [Option.t]. *)

  val either :
    ?distribution:float -> 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Either.t t
  (** Generator for [Either.t]. *)

  val try_ : ?distribution:float -> 'a t -> 'a Preface_stdlib.Try.t t
  (** Generator for [Try.t]. *)

  val validation :
    ?distribution:float -> 'a t -> 'a Preface_stdlib.Validation.t t
  (** Generator for [Validation.t]. *)

  val identity : 'a t -> 'a Preface_stdlib.Identity.t t
  (** Generator for [Identity.t]. *)

  val exn : exn t
  (** Generator of [exn]. *)

  val continuation : 'a t -> 'a Preface_stdlib.Continuation.t t
  (** Generator for [Continuation.t].*)

  val stream : 'a t -> 'a Preface_stdlib.Stream.t t
  (** Generator for [Stream.t].*)

  (** {2 Gen exception} *)

  exception A

  exception B

  exception C of int

  exception D of float

  exception E of string
end

(** Additionnal Arbitrary *)
module Arbitrary : sig
  type 'a t = 'a QCheck.arbitrary

  val identity : 'a t -> 'a Preface_stdlib.Identity.t t
  (** Arbitrary for [Identity.t] *)

  val either : 'a t -> 'b t -> ('a, 'b) Preface_stdlib.Either.t t
  (** Arbitrary for [Either.t]. *)

  val try_ : 'a t -> 'a Preface_stdlib.Try.t t
  (** Arbitrary for [Try.t]. *)

  val validation : 'a t -> 'a Preface_stdlib.Validation.t t
  (** Arbitrary for [Try.t]. *)

  val continuation : 'a t -> 'a Preface_stdlib.Continuation.t t
  (** Arbitrary for [Continuation.t].*)

  val stream : 'a t -> 'a Preface_stdlib.Stream.t t
  (** Arbitrary for [Stream.t].*)
end

(** {2 Helper as sample} *)

module Sample : sig
  module Int : GENERATOR with type input = int

  module String : GENERATOR with type input = string
end
