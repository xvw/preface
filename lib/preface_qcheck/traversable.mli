(** Functors that generate a suite for a [Traversable]. *)

module Suite_monad
    (R : Model.COVARIANT_1)
    (T : Preface_specs.Traversable.API_OVER_MONAD with type 'a t = 'a R.t)
    (A : Model.T0) : Model.SUITE

module Suite_applicative
    (R : Model.COVARIANT_1)
    (T : Preface_specs.Traversable.API_OVER_APPLICATIVE with type 'a t = 'a R.t)
    (RF : Model.COVARIANT_1)
    (F : Preface_specs.APPLICATIVE with type 'a t = 'a RF.t)
    (RG : Model.COVARIANT_1)
    (G : Preface_specs.APPLICATIVE with type 'a t = 'a RG.t) (NT : sig
      val run : 'a F.t -> 'a G.t
    end)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) : Model.SUITE
