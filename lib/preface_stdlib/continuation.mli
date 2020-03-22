(** Exposes [Continuation.Make(...).t]. A continuation-passing style - aka CPS -
    function takes an additional argument, a continuation. Then instead of
    returning a value, it applies the continuation to the value.

    For instance arithmetic operations can be transposed using CPS approach.

    {[
      val add : int -> int -> { run: 'b. (int -> 'b) -> 'b }

      let add x y = pure (x + y)
    ]}

    The continuation habits the type [{ run : 'b. (int -> 'b) -> 'b}] which is
    the specialization of the following generalized from
    [{run : 'b. ('a -> 'b) -> 'b}].

    The continuation [Functor], [Applicative] and [Monad] gives us the
    capability to manipulate such functions thanks to corresponding functions
    like [map], [apply], [bind] etc.

    {2 Contruction}

    The first step consists in building a continuation module specifying the
    result type. This is the role of the module [Continuation].

    {[ module Continuation = Preface_stdlib.Continuation ]}

    Therefore the module [Continuation] exposes the continuation type
    [{run : 'b. ('a -> 'b) -> 'b}]. It also provides modules like [Functor],
    [Applicative] and [Monad].

    {2 Example}

    {[
      module Continuation = Preface_stdlib.Continuation

      let delta b a c =
        let open Continuation in
        let open Continuation.Applicative in
        let add x y = pure (x + y)
        and mult x y = pure (x * y) in
        let+ bb = mult b b
        and+ a4 = mult a 4
        and+ c = pure c in
        bb - (a4 * c)
      ;;
    ]} *)

type 'a t = { run : 'r. ('a -> 'r) -> 'r }
(** {1 Type} *)

val pure : 'a -> 'a t
(** {2 Promotion} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)
