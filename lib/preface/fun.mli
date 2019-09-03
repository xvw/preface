(** Work with functions. *)

(** {1 Function composition} *)

val compose_left_to_right : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [(compose_left_to_right f g) x] is [g (f x)].  *)

val compose_right_to_left : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** [(compose_right_to_left f g) x] is [f (g x)]. It is the
    mathematical composition.
*)

(** {1 Infix operators} *)

module Infix : sig
  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** [(f %> g) x] is [g (f x)]. Alias for {!val:compose_left_to_right} *)

  val ( <% ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
  (** [(f <% g) x] is [f (g x)]. Alias for {!val:compose_right_to_left} *)
end

(** {1 Shadowing the [Stdlib]} *)

include module type of Stdlib.Fun

include module type of Infix
