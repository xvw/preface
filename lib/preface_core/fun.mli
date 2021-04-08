val compose_left_to_right : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [(compose_left_to_right f g) x] is [g (f x)]. *)

val compose_right_to_left : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [(compose_right_to_left f g) x] is [f (g x)]. It is the mathematical
    composition. *)

val const : 'a -> 'b -> 'a
(** Produce a function that returns its first argument. [const a b] returns
    always [a]. *)

val id : 'a -> 'a
(** Identity function. *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip f x y] is [f y x]. *)

(** {2 Infix operators} *)

module Infix : sig
  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** [(f %> g) x] is [g (f x)]. Alias for
      {!val:Preface_stdlib.Fun.compose_left_to_right} *)

  val ( <% ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** [(f <% g) x] is [f (g x)]. Alias for
      {!val:Preface_stdlib.Fun.compose_right_to_left} *)

  val ( % ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
  (** [(f <% g) x] is [f (g x)]. Alias for
      {!val:Preface_stdlib.Fun.compose_right_to_left} *)
end

include module type of Infix
(** @inline *)
