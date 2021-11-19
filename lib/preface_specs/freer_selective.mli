(** A [Freer selective] allows you to build a {e rigid}
    {!module:Preface_specs.Selective} from a given arbitrary type. *)

(** Such {!module:Preface_specs.selective} is equiped with and additional
    function for [promoting] values from the underlying type into the
    [Freer selective] and a [Natural transformations] for transforming the value
    of the [Freer selective] to an other {!module:Preface_specs.Selective} or to
    a {!module:Preface_specs.Monoid}. *)

(** {1 Structure anatomy} *)

(** The natural transformation for [Freer Selective] to [Selective]. *)
module type TO_SELECTIVE = sig
  type 'a t
  (** The type held by the [Freer Selective]. *)

  type 'a f
  (** The parametric type (which, unlike a
      {!module:Preface_specs.Free_selective} don't need to be a
      {!module:Preface_specs.Functor}). *)

  type 'a selective
  (** The type held by the [Selective]. *)

  type natural_transformation = { transform : 'a. 'a f -> 'a selective }

  val run : natural_transformation -> 'a t -> 'a selective
  (** Run the natural transformation over the [Freer selective]. *)
end

(** The natural transformation for [Freer Selective] to [Monoid]. *)
module type TO_MONOID = sig
  type 'a t
  (** The type held by the [Freer selective]. *)

  type 'a f
  (** The parametric type (which, unlike a
      {!module:Preface_specs.Free_selective} don't need to be a
      {!module:Preface_specs.Functor}). *)

  type monoid
  (** The type held by the [Monoid]. *)

  type natural_transformation = { transform : 'a. 'a f -> monoid }

  val run : natural_transformation -> 'a t -> monoid
  (** Run the natural transformation over the [Freer selective]. *)
end

(** The [Freer selective] API without the {!module:Preface_specs.Selective} API. *)
module type CORE = sig
  type 'a f
  (** The parametric type (which, unlike a
      {!module:Preface_specs.Free_selective} don't need to be a
      {!module:Preface_specs.Functor}). *)

  (** The type held by the [Freer selective]. *)
  type _ t =
    | Pure : 'a -> 'a t
    | Select : (('b -> 'a, 'a) Either.t t * 'b f) -> 'a t

  val promote : 'a f -> 'a t
  (** Promote a value from the {!module:Preface_specs.Functor} into the
      [Freer selective]. *)

  (** The natural transformation from a [Freer selective] to an other
      {!module:Preface_specs.Selective}. *)
  module To_selective (Selective : Selective.API) :
    TO_SELECTIVE
      with type 'a t := 'a t
       and type 'a f := 'a f
       and type 'a selective := 'a Selective.t

  (** The natural transformation from a [Freer selective] to a
      {!module:Preface_specs.Monoid}. *)
  module To_monoid (Monoid : Monoid.CORE) :
    TO_MONOID
      with type 'a t := 'a t
       and type 'a f := 'a f
       and type monoid := Monoid.t
end

(** {1 Complete API} *)

(** The complete interface of a [Freer selective]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Selective API}

      A [Freer selective] is also a {!module:Preface_specs.Selective}. *)

  include Selective.API with type 'a t := 'a t
  (** @inline *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
