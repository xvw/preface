(** Work with functions. *)

(** {1 Function composition} *)

val compose_left_to_right : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [(compose_left_to_right f g) x] is [g (f x)]. *)

val compose_right_to_left : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [(compose_right_to_left f g) x] is [f (g x)]. It is the mathematical
    composition. *)

val constant : 'a -> 'b -> 'a
(** Produce a function that returns its first argument. [const a b] returns
    always [a]. *)

(** {1 Infix operators} *)

module Infix : sig
  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** [(f %> g) x] is [g (f x)]. Alias for {!val:compose_left_to_right} *)

  val ( <% ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** [(f <% g) x] is [f (g x)]. Alias for {!val:compose_right_to_left} *)
end

(** {1 Shadowing the [Stdlib]} *)

include module type of Stdlib.Fun

include module type of Infix
