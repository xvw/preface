(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Writer module gives you the ability to accumulate values thanks to the
    parametric `Monoid`. Then for instance it can be used to create a log
    reflecting performed operations.

    {1 Example} *)

module Over (W : Preface_specs.MONOID) : sig

  type output = W.t

  type 'a t

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)

  val run : 'a t -> 'a * output
  (* [run] execute the effect and returns the result and the corresponding output *)

  val write : ('a * output) -> 'a t
  (* [write] embeds an output and provides the corresponding effect *)

  val tell : output -> unit t
  (* [tell] helps to enrich the output. This is done thanks to the semigroup combine operation *)

  val listen : 'a t -> ('a * output) t
  (* [listen] executes the effect and return both the result and the corresponding output within the effect *)

  val pass : ('a * (output -> output)) t -> 'a t
  (* [pass] executes the effect and apply the function to the corresponding output *)

  val listens : (output -> 'b) -> 'a t -> ('a * 'b) t
  (* [listens] executes the effect and returns the result and the transformation of the output thanks to the parametric function *)
end
