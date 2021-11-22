(** [Cokleisli] uses the Cokleisli category to describe arity 2 constructions
    for arity 1 constructions, usually using the form:
    [type ('a, 'b) t = 'a F.t -> 'b]. *)

(** {2 Bifunctor from a Contravariant}

    Produces a {!module-type:Preface_specs.BIFUNCTOR} from a
    {!module-type:Preface_specs.Contravariant}. *)

module Bifunctor (C : Preface_specs.Contravariant.CORE) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = 'a C.t -> 'b

(** {2 Profunctor from a Functor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} from a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Profunctor (F : Preface_specs.Functor.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a F.t -> 'b

(** {2 Strong from a Comonad}

    Produces a {!module-type:Preface_specs.STRONG} from a
    {!module-type:Preface_specs.COMONAD}. *)

module Strong (C : Preface_specs.Comonad.CORE) :
  Preface_specs.STRONG with type ('a, 'b) t = 'a C.t -> 'b

(** {2 Closed from a Functor}

    Produces a {!module-type:Preface_specs.CLOSED} from a
    {!module-type:Preface_specs.FUNCTOR}. *)

module Closed (F : Preface_specs.Functor.CORE) :
  Preface_specs.CLOSED with type ('a, 'b) t = 'a F.t -> 'b

(** {2 Category from a Comonad}

    Produces a {!module-type:Preface_specs.CATEGORY} from a
    {!module-type:Preface_specs.COMONAD}. *)

module Category (C : Preface_specs.Comonad.CORE) :
  Preface_specs.CATEGORY with type ('a, 'b) t = 'a C.t -> 'b

(** {2 Arrow from a Comonad}

    Produces a {!module-type:Preface_specs.ARROW} from a
    {!module-type:Preface_specs.COMONAD}. *)

module Arrow (C : Preface_specs.Comonad.CORE) :
  Preface_specs.ARROW with type ('a, 'b) t = 'a C.t -> 'b
