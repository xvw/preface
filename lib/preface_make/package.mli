(** Package multiple API together. *)

(** Pack Applicative and Monad. *)
module From_applicative_and_monad
    (A : Preface_specs.APPLICATIVE)
    (M : Preface_specs.MONAD with type 'a t = 'a A.t) :
  Preface_specs.Package.APPLICATIVE_AND_MONAD with type 'a t = 'a M.t

(** Pack Alternative and Monad_plus. *)
module From_alternative_and_monad_plus
    (A : Preface_specs.ALTERNATIVE)
    (M : Preface_specs.MONAD_PLUS with type 'a t = 'a A.t) :
  Preface_specs.Package.ALTERNATIVE_AND_MONAD_PLUS with type 'a t = 'a M.t
