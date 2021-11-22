(** [Kleisli] uses the Kleisli category to describe arity 2 constructions for
    arity 1 constructions, usually using the form:
    [type ('a, 'b) t = 'a -> F.t]. *)

(** {2 Profunctor from a Functor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Profunctor (Functor : Preface_specs.Functor.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a -> 'b Functor.t

(** {2 Choice From an Applicative}

    Produces a {!module-type:Preface_specs.CHOICE} from a
    {!module-type:Preface_specs.APPLICATIVE}. *)

module Choice (Applicative : Preface_specs.Applicative.CORE) :
  Preface_specs.CHOICE with type ('a, 'b) t = 'a -> 'b Applicative.t

(** {2 Strong from a Monad}

    Produces a {!module-type:Preface_specs.STRONG} from a
    {!module-type:Preface_specs.MONAD}. (Using Star)*)

module Strong (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Category from a Monad}

    Produces a {!module-type:Preface_specs.CATEGORY} from a
    {!module-type:Preface_specs.MONAD}. *)

module Category (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow from a Monad}

    Produces an {!module-type:Preface_specs.ARROW} from a
    {!module-type:Preface_specs.MONAD} (using the [Kleisli Arrow]). *)

module Arrow (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow zero from a Monad Plus}

    Produces an {!module-type:Preface_specs.ARROW_ZERO} from a
    {!module-type:Preface_specs.MONAD_PLUS} (using the [Kleisli Arrow]). *)

module Arrow_zero (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_ZERO with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow alt from a Monad Plus}

    Produces an {!module-type:Preface_specs.ARROW_ALT} from a
    {!module-type:Preface_specs.MONAD_PLUSD} (using the [Kleisli Arrow]). *)

module Arrow_alt (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_ALT with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow plus from a Monad Plus}

    Produces an {!module-type:Preface_specs.ARROW_PLUS} from a
    {!module-type:Preface_specs.MONAD_PLUS} (using the [Kleisli Arrow]). *)

module Arrow_plus (Monad : Preface_specs.Monad_plus.CORE) :
  Preface_specs.ARROW_PLUS with type ('a, 'b) t = 'a -> 'b Monad.t

(** {2 Arrow Choice from a Monad}

    Produces an {!module-type:Preface_specs.ARROW_CHOICE} from a
    {!module-type:Preface_specs.MONAD} (using the [Kleisli Arrow]). *)

module Arrow_choice (Monad : Preface_specs.Monad.CORE) :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = 'a -> 'b Monad.t
