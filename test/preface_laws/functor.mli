(** Test cases for [Functors] *)

module Cases
    (F : Preface_specs.Functor.CORE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (T : Preface_qcheck.Sample.PACKAGE) : sig
  val cases : int -> unit Alcotest.test_case list
end

(** {2 Specific tests}

    Some tests used outside of the scope of Functor. *)

module Preserve_identity
    (F : Preface_specs.Functor.CORE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (X : Preface_qcheck.Model.T0) : sig
  val test : int -> QCheck.Test.t
end

module Preserve_morphism
    (F : Preface_specs.Functor.CORE)
    (A : Preface_qcheck.Model.T1 with type 'a t = 'a F.t)
    (X : Preface_qcheck.Model.T0)
    (Y : Preface_qcheck.Model.T0)
    (Z : Preface_qcheck.Model.T0) : sig
  val test : int -> QCheck.Test.t
end
