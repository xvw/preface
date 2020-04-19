(** [Requirements] exposes module types involved by some interfaces of the
    [Specs] library. These interfaces make it possible not to integrate concrete
    code in the library. Keeping [Specs] as abstract as possible. *)

(** [EITHER] hold a types which represents values with two possibilites. *)
module type EITHER = sig
  type ('a, 'b) t
  (** The type representing the disjonction. *)

  val left : 'a -> ('a, 'b) t
  (** Creates a disjonction with first case. *)

  val right : 'b -> ('a, 'b) t
  (** Creates a disjonction with second case. *)

  val map_left : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  (** Mapping over the first value of the disjonction. *)

  val map_right : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second value of the disjonction. *)

  val case : ('a, 'b) t -> ('a -> 'c) -> ('b -> 'c) -> 'c
  (** Mapping over first or second value of the disjonction. *)
end
