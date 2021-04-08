(** A Non empty list. The module allows to deal with non-empty list. Lists where
    the minimum size is one.*)

type 'a t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

val create : 'a -> 'a t
(** [create x] create a new non-empty list with [x]. *)

val from_list : 'a list -> 'a t option
(** Creates a non-empty list from a regular list. *)

val to_list : 'a t -> 'a list
(** Convert non-empty list to a regular list. *)

val hd : 'a t -> 'a
(** Returns the head of a non-empty list. *)

val tl : 'a t -> 'a t option
(** Returns the tail of a non-empty list. *)

val length : 'a t -> int
(** Returns the length of a non-empty list. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is add [x] as head of [xs]. *)

val append : 'a t -> 'a t -> 'a t
(** [append a b] concat [a] and [b]. *)

val rev : 'a t -> 'a t
(** [rev xs] reverse [xs]. *)

val rev_append : 'a t -> 'a t -> 'a t
(** [rev_append a b] is a tail-recursive version of [append (rev a) b]. *)

val flatten : 'a t t -> 'a t
(** Concat a non-empty list of non-empty list *)

val iter : ('a -> unit) -> 'a t -> unit
(** {!val:List.iter} for non-empty list. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** {!val:List.iteri} for non-empty list. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** {!val:List.map} for non-empty list. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** {!val:List.mapi} for non-empty list. *)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** [reduce f xs] reduce all value of [xs] into one. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** {!val:List.fold_left} for non-empty list. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** {!val:List.fold_right} for non-empty list. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality between [Nonempty_list.t].*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Formatter for pretty-printing for [Nonempty_list.t]. *)
