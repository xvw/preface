module type CORE_WITH_FOLD_MAP = sig
  type 'a t

  val fold_map' : 'a -> ('a -> 'a -> 'a) -> ('b -> 'a) -> 'b t -> 'a
end

module type CORE_WITH_FOLD_RIGHT = sig
  type 'a t

  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type CORE = sig
  include CORE_WITH_FOLD_MAP

  include CORE_WITH_FOLD_RIGHT with type 'a t := 'a t
end

module type OPERATION = sig
  type 'a t

  val reduce : (module Monoid.API with type t = 'm) -> 'm t -> 'm

  val fold_map :
    (module Monoid.API with type t = 'm) -> ('a -> 'm) -> 'a t -> 'm

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  val for_all : ('a -> bool) -> 'a t -> bool

  val exists : ('a -> bool) -> 'a t -> bool

  val length : 'a t -> int
end

module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t
end
