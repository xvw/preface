(** A [Monad] - TODO *)

module type CORE = sig
  type 'a t
end

module type OPERATION = sig
  type 'a t
end

module type SYNTAX = sig
  type 'a t
end

module type INFIX = sig
  type 'a t
end

(** {1 API} *)

(** The complete interface of a [Monad]. *)
module type API = sig
  include CORE

  include OPERATION with type 'a t := 'a t

  module Syntax : SYNTAX with type 'a t := 'a t

  include module type of Syntax

  module Infix : INFIX with type 'a t := 'a t

  include module type of Infix
end
