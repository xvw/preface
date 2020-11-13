(** A generic monoid. *)

val times : ('a -> 'a -> 'a) -> int -> 'a -> 'a option
(** [times combine n x] apply [combine] on [x] [n] times. If [n] is lower than
    [1] the function will returns [None] . *)

val reduce_nel : ('a -> 'a -> 'a) -> 'a Nonempty_list.t -> 'a
(** [reduce_nel combine list] Reduce a [Nonempty_list.t] using [combine]. *)

val reduce : ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a
(** [reduce combine neutral list] Reduce a [list] using [combine] and [neutral]
    as initial value.*)
