(** An [Applicative] for [('a -> 'b) t] is TODO.
*)

(** {1 Core} *)

(** Standard requirement. *)
module type Core = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val pure : 'a -> 'a t
  (** Create a new ['a t]. *)

  val product : 'a t -> 'b t -> 'a * 'b t
  (** TODO *)
end

(** {1 Operation} *)

module type Operation = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ap : ('a -> 'b) t -> 'a t -> 'b t
  (** TODO *)

  val liftA : ('a -> 'b) -> 'a t -> 'b t
  (** TODO *)

  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** TODO *)

  val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** TODO *)
end

(** {1 Syntax} *)

module type Syntax = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** TODO *)

  val ( and+ ) : 'a t -> 'b t -> 'a * 'b t
  (** TODO *)
end

(** {1 Infix} *)

module type Infix = sig
  type 'a t
  (** The type holded by the [Applicative]. *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** TODO *)

  val ( <**> ) : 'a t -> ('a -> 'b) t -> 'b t
  (** TODO *)

  val ( <* ) : 'a t -> 'b t -> 'a t
  (** TODO *)

  val ( *> ) : 'a t -> 'b t -> 'b t
  (** TODO *)
end
