(** Sampling data. *)

(** {2 Presaved generators} *)

module Int : Model.T0 with type t = int

module String : Model.T0 with type t = string

module Float : Model.T0 with type t = float

(** {2 Packaged Samples} *)

module type PACKAGE = sig
  module A : Model.T0

  module B : Model.T0

  module C : Model.T0

  module D : Model.T0

  module E : Model.T0

  module F : Model.T0
end

module Pack1 : PACKAGE
