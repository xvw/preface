(** Building a {!module:Preface_specs.Functor} *)

(** {1 Using the minimal definition}

    Build a {!module-type:Preface_specs.FUNCTOR} using
    {!module-type:Preface_specs.Functor.WITH_MAP}.

    Standard method, using the minimal definition of a functor to derive its
    full API. *)

module Via_map (Req : Preface_specs.Functor.WITH_MAP) :
  Preface_specs.FUNCTOR with type 'a t = 'a Req.t

(** {1 Functor Algebra}

    Construction of {!module-type:Preface_specs.FUNCTOR} by combining two other
    {!module-type:Preface_specs.FUNCTOR}. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.FUNCTOR}.*)

module Composition (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) :
  Preface_specs.FUNCTOR with type 'a t = 'a G.t F.t

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.FUNCTOR}. *)

module Product (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) :
  Preface_specs.FUNCTOR with type 'a t = 'a F.t * 'a G.t

(** {2 Sum}

    Sum of {!module-type:Preface_specs.FUNCTOR} using the technique described in
    {{:http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf} Data
    types à la carte by W. Swierstra}.*)

module Sum (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) : sig
  type 'a sum =
    | L of 'a F.t
    | R of 'a G.t

  include Preface_specs.FUNCTOR with type 'a t = 'a sum
end

(** {1 From other abstraction} *)

(** {2 From an Arrow}

    Specialize an {!module-type:Preface_specs.ARROW} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_arrow (A : Preface_specs.ARROW) :
  Preface_specs.FUNCTOR with type 'a t = (unit, 'a) A.t

(** {2 From an Applicative}

    Specialize an {!module-type:Preface_specs.APPLICATIVE} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_applicative (Applicative : Preface_specs.APPLICATIVE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Applicative.t

(** {2 From an Alt}

    Specialize an {!module-type:Preface_specs.ALT} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_alt (Alt : Preface_specs.ALT) :
  Preface_specs.FUNCTOR with type 'a t = 'a Alt.t

(** {2 From a Monad}

    Specialize a {!module-type:Preface_specs.MONAD} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.FUNCTOR with type 'a t = 'a Monad.t

(** {2 From an Alternative}

    Specialize an {!module-type:Preface_specs.ALTERNATIVE} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Alternative.t

(** {2 From a Monad plus}

    Specialize a {!module-type:Preface_specs.MONAD_PLUS} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.FUNCTOR with type 'a t = 'a Monad_plus.t

(** {2 From a Comonad}

    Specialize a {!module-type:Preface_specs.COMONAD} into a
    {!module-type:Preface_specs.FUNCTOR}. *)

module From_comonad (Comonad : Preface_specs.COMONAD) :
  Preface_specs.FUNCTOR with type 'a t = 'a Comonad.t

(** {2 From a Bifunctor}

    Specialize a {!module-type:Preface_specs.BIFUNCTOR} into a
    {!module-type:Preface_specs.FUNCTOR} using Join. *)

module From_bifunctor (Bifunctor : Preface_specs.Bifunctor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = ('a, 'a) Bifunctor.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.FUNCTOR}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.FUNCTOR}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.FUNCTOR with type 'a t = 'a Core.t

(** {2 Building Core} *)

module Core (Req : Preface_specs.Functor.WITH_MAP) :
  Preface_specs.Functor.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Functor.CORE) :
  Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t

(** {2 Deriving Infix} *)

module Infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Functor.INFIX with type 'a t = 'a Core.t
