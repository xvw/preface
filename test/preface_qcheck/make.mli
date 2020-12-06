(** Describes a Test driver. *)
module type DRIVER = sig
  type input

  type output

  val arbitrary : input QCheck.arbitrary

  val left : input -> output

  val right : input -> output

  val equal : output -> output -> bool

  val name : string
end

(** Generate a property basing test from a Driver *)
module Test (T : DRIVER) : sig
  val test : int -> QCheck.Test.t
end
