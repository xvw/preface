(** Generate a suite for [Preface_specs.COMONAD]. *)

module Make_hooked
    (C : Preface_specs.COMONAD)
    (R : Requirement.INPUT_T1 with type 'a t = 'a C.t)
    (Hook : Requirement.HOOK with type 'a t = 'a C.t)
    (Obs : sig
      type 'a t

      val f : 'a QCheck.Observable.t -> 'a t QCheck.Observable.t
    end
    with type 'a t = 'a C.t)
    (P : Sample.PACK) : Requirement.OUTPUT
