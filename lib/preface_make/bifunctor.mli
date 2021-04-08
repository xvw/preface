(** Building a {!module:Preface_specs.Bifunctor} *)

(** {1 Using the minimal definition} *)

(** {2 Using bimap}

    Build a {!module-type:Preface_specs.BIFUNCTOR} using
    {!module-type:Preface_specs.Bifunctor.WITH_BIMAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_bimap (Req : Preface_specs.Bifunctor.WITH_BIMAP) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Using map_fst and map_snd}

    Build a {!module-type:Preface_specs.BIFUNCTOR} using
    {!module-type:Preface_specs.Bifunctor.WITH_MAP_FST_AND_MAP_SND}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_map_fst_and_map_snd
    (Req : Preface_specs.Bifunctor.WITH_MAP_FST_AND_MAP_SND) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 From the product of two functors}

    Build a {!module-type:Preface_specs.BIFUNCTOR} using the product of two
    {!module-type:Preface_specs.FUNCTOR}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module From_functors_product
    (F : Preface_specs.FUNCTOR)
    (G : Preface_specs.FUNCTOR) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = 'a F.t * 'b G.t

(** {2 From the sum of two functors}

    Build a {!module-type:Preface_specs.BIFUNCTOR} using the sum of two
    {!module-type:Preface_specs.FUNCTOR} using the technique described in
    {{:http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf} Data
    types à la carte by W. Swierstra}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module From_functors_sum (F : Preface_specs.FUNCTOR) (G : Preface_specs.FUNCTOR) : sig
  type ('a, 'b) sum =
    | L of 'a F.t
    | R of 'b G.t

  include Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) sum
end

(** {1 Bifunctor Algebra}

    Construction of {!module-type:Preface_specs.BIFUNCTOR} by combining them. *)

(** {2 Product}

    Construct the product of two {!module-type:Preface_specs.BIFUNCTOR}. *)

module Product (F : Preface_specs.BIFUNCTOR) (G : Preface_specs.BIFUNCTOR) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) F.t * ('a, 'b) G.t

(** {2 Sum}

    Sum of {!module-type:Preface_specs.BIFUNCTOR} using the technique described
    in {{:http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf}
    Data types à la carte by W. Swierstra}.*)

module Sum (F : Preface_specs.BIFUNCTOR) (G : Preface_specs.BIFUNCTOR) : sig
  type ('a, 'b) sum =
    | L of ('a, 'b) F.t
    | R of ('a, 'b) G.t

  include Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) sum
end

(** {1 Manual construction}

    Advanced way to build an {!module-type:Preface_specs.BIFUNCTOR},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.BIFUNCTOR}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (Core : Preface_specs.Bifunctor.CORE)
    (Operation : Preface_specs.Bifunctor.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = ('a, 'b) Core.t

(** {2 Building Core} *)

module Core_via_bimap (Req : Preface_specs.Bifunctor.WITH_BIMAP) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t

module Core_via_map_fst_and_map_snd
    (Req : Preface_specs.Bifunctor.WITH_MAP_FST_AND_MAP_SND) :
  Preface_specs.Bifunctor.CORE with type ('a, 'b) t = ('a, 'b) Req.t

(** {2 Deriving Operation} *)

module Operation (Core : Preface_specs.Bifunctor.CORE) :
  Preface_specs.Bifunctor.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t
