(** A [Store comonad] parametrized over an inner {!module:Comonad} and a store
    (which is an arbitrary type). [Store] is a {e comonad transformer}. [Store]
    is [Costate] (the dual of [State]).*)

(** Operation of [Store] comonad parametrized over an inner comonad and [store]. *)
module type CORE = sig
  type store
  (** The encapsulated store. *)

  type 'a comonad
  (** The inner comonad. *)

  type 'a t = (store -> 'a) comonad * store
  (** The type held by the store comonad.*)

  (** @inline *)
  include
    Comonad.TRANSFORMER with type 'a t := 'a t and type 'a comonad := 'a comonad

  val run : 'a t -> (store -> 'a) comonad * store
  (** Unwrap the store computation. *)

  val pos : 'a t -> store
  (** Read the stored value. *)

  val peek : store -> 'a t -> 'a
  (** Get the current focus for a different stored value (absolute getter). *)

  val peeks : (store -> store) -> 'a t -> 'a
  (** Get the current focus for a different modified stored value (absolute
      setter). *)

  val seek : store -> 'a t -> 'a t
  (** Move the current focus (absolute setter). *)

  val seeks : (store -> store) -> 'a t -> 'a t
  (** Modify (set and apply a function) the focus (relative setter). *)

  (** Applies a function which lift the store to a functor-value and use the
      accessor to read the resulting focus. *)
  module Experiment (F : Functor.API) : sig
    val run : (store -> store F.t) -> 'a t -> 'a F.t
    (** Perform the accessor for reading the resulting focus. *)
  end
end

(** {1 Complete API} *)

(** The complete interface of a [Store] comonad wich introduces the
    {!module:Comonad} API into the [Store] API. *)
module type API = sig
  include CORE
  (** @inline *)

  (** {2 Comonad} *)

  module Comonad : Comonad.API

  include module type of Comonad with type 'a t := 'a t
  (** @inline *)
end
