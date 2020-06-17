(** Sampling data. *)

(** {2 QCheck generator} *)

module type GENERATOR = sig
  type input

  val arbitrary : input Arbitrary.t

  val observable : input QCheck.Observable.t

  val eq : input -> input -> bool
end

module type PACK = sig
  module T1 : GENERATOR

  module T2 : GENERATOR

  module T3 : GENERATOR

  module T4 : GENERATOR

  module T5 : GENERATOR

  module T6 : GENERATOR
end

(** {2 Presaved generators} *)

module Int : GENERATOR with type input = int

module String : GENERATOR with type input = string

module Float : GENERATOR with type input = float

(** {2 Packed generators} *)

module Pack : sig
  module T1 : GENERATOR with type input = int

  module T2 : GENERATOR with type input = string

  module T3 : GENERATOR with type input = float

  module T4 : GENERATOR with type input = int

  module T5 : GENERATOR with type input = string

  module T6 : GENERATOR with type input = float
end
