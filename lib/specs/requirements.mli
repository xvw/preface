(** [Requirements] exposes module types involved by some interfaces of 
    the [Specs] library. These interfaces make it possible not to 
    integrate concrete code in the library. 
    Keeping [Specs] as abstract as possible.
*)

(** [EITHER] hold a types which represents values with two possibilites.
*)
module type EITHER = sig
  type ('a, 'b) t
  (** The type representing the disjonction. *)

  val left : 'a -> ('a, 'b) t
  (** Creates a disjonction with first case. *)

  val right : 'b -> ('a, 'b) t
  (** Creates a disjonction with second case. *)

  val left_of : ('a, 'b) t -> 'a
  (** Get the first value of the disjonction. *)

  val right_of : ('a, 'b) t -> 'b
  (** Get the second value of the disjonction. *)

  val map_left : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the first value of the disjonction. *)

  val map_right : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second value of the disjonction. *)

  val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  (** Mapping over the second value of the disjonction. 
      
      alias of {!val:map_right} by convention.
  *)

  val map_both : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  (** Mapping over the two values of the disjonction. *)
end
