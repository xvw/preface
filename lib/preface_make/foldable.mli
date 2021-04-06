(** Building a {!module:Preface_specs.Foldable} *)

(** {1 Using the minimal definition} *)

(** {2 Using fold_map}

    Build a {!module-type:Preface_specs.FOLDABLE} using
    {!module-type:Preface_specs.Foldable.WITH_FOLD_MAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_fold_map (Req : Preface_specs.Foldable.WITH_FOLD_MAP) :
  Preface_specs.FOLDABLE with type 'a t = 'a Req.t

(** {2 Using fold_right}

    Build a {!module-type:Preface_specs.FOLDABLE} using
    {!module-type:Preface_specs.Foldable.WITH_FOLD_RIGHT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_fold_right (Req : Preface_specs.Foldable.WITH_FOLD_RIGHT) :
  Preface_specs.FOLDABLE with type 'a t = 'a Req.t

(** {1 Monad Algebra}

    Construction of {!module-type:Preface_specs.FOLDABLE} by combining them. *)

(** {2 Composition}

    Right-to-left composition of {!module-type:Preface_specs.FOLDABLE}.*)

module Composition (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) :
  Preface_specs.FOLDABLE with type 'a t = 'a G.t F.t

(** {2 Sum}

    Sum of {!module-type:Preface_specs.FOLDABLE} using the technique described
    in {{:http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf}
    Data types à la carte by W. Swierstra}.*)

module Sum (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) : sig
  type 'a sum =
    | L of 'a F.t
    | R of 'a G.t

  include Preface_specs.FOLDABLE with type 'a t = 'a sum
end

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.FOLDABLE}. *)

module Product (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t * 'a G.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.FOLDABLE}, constructing
    and assembling a component-by-component of
    {!module-type:Preface_specs.FOLDABLE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (C : Preface_specs.Foldable.CORE)
    (O : Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t) :
  Preface_specs.FOLDABLE with type 'a t = 'a O.t

(** {2 Building Core} *)

module Core_via_fold_right (Req : Preface_specs.Foldable.WITH_FOLD_RIGHT) :
  Preface_specs.Foldable.CORE with type 'a t = 'a Req.t

module Core_via_fold_map (Req : Preface_specs.Foldable.WITH_FOLD_MAP) :
  Preface_specs.Foldable.CORE with type 'a t = 'a Req.t

(** {2 Deriving Operation} *)

module Operation (C : Preface_specs.Foldable.CORE) :
  Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t
