(** A [Free selective] allows you to build a {e rigid} {!module:Preface_specs.Selective} from
    a given {!module:Preface_specs.Functor}. *)

(** Such {!module:Preface_specs.selective} is equiped with and additional function for
    [promoting] values from the underlying {!module:Preface_specs.Functor} into the
    [Free selective] and a [Natural transformations] for transforming the value
    of the [Free selective] to an other {!module:Preface_specs.Selective} or to a
    {!module:Preface_specs.Monoid}. *)

(** {2 Note about complexity}

    Although free constructs are elegant, they introduce an execution cost due
    to the recursive nature of defining the type of a [Free Selective]. There
    are {e cheaper} encodings but they are not, for the moment, available in
    Preface. *)

(** {1 Structure anatomy} *)

(** The [Free selective] API without the {!module:Preface_specs.Selective} API. *)
module type CORE = sig
  type 'a f
  (** The type held by the {!module:Preface_specs.Functor}. *)

  (** The type held by the [Free selective]. *)

  type _ t =
    | Pure : 'a -> 'a t
    | Select : ('a, 'b) Either.t t * ('a -> 'b) f -> 'b t

  val promote : 'a f -> 'a t
  (** Promote a value from the {!module:Preface_specs.Functor} into the [Free selective]. *)

  (** The natural transformation from a [Free selective] to an other
      {!module:Preface_specs.Selective}. *)
  module To_selective (Selective : Selective.CORE) : sig
    type natural_transformation = { transform : 'a. 'a f -> 'a Selective.t }

    val run : natural_transformation -> 'a t -> 'a Selective.t
    (** Run the natural transformation over the [Free selective]. *)
  end

  (** The natural transformation from a [Free selective] to a {!module:Preface_specs.Monoid}. *)
  module To_monoid (Monoid : Monoid.CORE) : sig
    type natural_transformation = { transform : 'a. 'a f -> Monoid.t }

    val run : natural_transformation -> 'a t -> Monoid.t
    (** Run the natural transformation over the [Free selective]. *)
  end
end

(** {1 Complete API} *)

(** The complete interface of a [Free selective]. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {1 Selective API}

      A [Free selective] is also a {!module:Preface_specs.Selective}. *)

  include Selective.API with type 'a t := 'a t
  (** @closed *)
end

(** {1 Additional references}

    - {{:http://hackage.haskell.org/package/selective} Haskell's documentation
      of a Selective Application Functor}
    - {{:https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf}
      Selective Applicative Functors} *)
