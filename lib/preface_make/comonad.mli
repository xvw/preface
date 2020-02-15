(** Modules for building [!Preface_specs.COMONAD] modules. *)

(** {1 Tutorial}

    As with [Functor], [Applicative] and [Monad], [Comonad] allow multiple
    pathes to build a [Comonad] module.

    {2 Basics}

    As [Monad], [Comonad] offer several primitive ways to be built. These
    different approaches lead to the same combiners. It's up to you to choose
    the most flexible approach according to the context.

    {3 Using [map] and [duplicate]}

    Here's an example with the [Stream] module implementing [Comonad] using
    [map] and [duplicate].

    {[
      (* In: stream.ml *)
      type 'a t = C of 'a * 'a t Lazy.t

      let stream a l = C (a, l)

      module Comonad = Preface_make.Monad.Via_map_and_duplicate (struct
        type 'a t = 'a t

        let extract = function C (a, _) -> a

        let rec duplicate = function
          | C (a, s) -> C (C (a, s), lazy (duplicate @@ Lazy.force s))
        ;;

        let rec map f = function
          | C (a, s) -> C (f a, lazy (map f @@ Lazy.force s))
        ;;
      end)
    ]}
    {[
      (* In: stream.mli *)
      type 'a t

      val stream : 'a -> 'a t Lazy.t -> 'a t

      module Comonad : Preface_specs.COMONAD with type 'a t = 'a t
    ]}

    Now, your [Stream] module is handling a [Comonad] module and you'll be be
    able to use features offered by a [Comonad]. For example:

    {[
      let natural =
        let rec natural n = stream n (lazy (natural (n + 1))) in
        natural 0
      ;;

      let natural_positive = increment <<= 1 + extract s
    ]}

    {3 Using [extend]}

    Same [Stream] example using [extend].

    {[
      (* In: stream.ml *)
      type 'a t = C of 'a * 'a t Lazy.t

      let stream a l = C (a, l)

      module Comonad = Preface_make.Monad.Via_map_and_duplicate (struct
        type 'a t = 'a t

        let extract = function C (a, _) -> a

        let rec extend f = function
          | C (_, s') as s -> C (f s, lazy (extend f @@ Lazy.force s'))
        ;;
      end)
    ]}

    {3 Using [compose_left_to_right]}

    Same [Stream] example using [compose_left_to_right].

    {[
      (* In: stream.ml *)
      type 'a t = C of 'a * 'a t Lazy.t

      let stream a l = C (a, l)

      module Comonad = Preface_make.Monad.Via_map_and_duplicate (struct
        type 'a t = 'a t

        let extract = function C (a, _) -> a

        let compose_left_to_right f g at =
          let rec extend f = function
            | C (_, s') as s -> C (f s, lazy (extend f @@ Lazy.force s'))
          in
          g @@ extend f at
        ;;
      end)
    ]} *)

(** {1 Construction of a [Comonad] module} *)

module Via_map_and_duplicate
    (Core : Preface_specs.Comonad.CORE_WITH_MAP_AND_DUPLICATE) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Via_extend (Core : Preface_specs.Comonad.CORE_WITH_EXTEND) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Via_cokleisli_composition
    (Core : Preface_specs.Comonad.CORE_WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

module Make
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Comonad.SYNTAX with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t) :
  Preface_specs.COMONAD with type 'a t = 'a Core.t

(** {1 Internal construction of a [Comonad] module} *)

module Core_via_map_and_duplicate
    (Core_with_map_and_duplicate : Preface_specs.Comonad
                                   .CORE_WITH_MAP_AND_DUPLICATE) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core_with_map_and_duplicate.t

module Core_via_extend
    (Core_with_extend : Preface_specs.Comonad.CORE_WITH_EXTEND) :
  Preface_specs.Comonad.CORE with type 'a t = 'a Core_with_extend.t

module Core_via_cokleisli_composition
    (Core_with_cokleisli_composition : Preface_specs.Comonad
                                       .CORE_WITH_COKLEISLI_COMPOSITION) :
  Preface_specs.Comonad.CORE
    with type 'a t = 'a Core_with_cokleisli_composition.t

module Operation (Core : Preface_specs.Comonad.CORE) :
  Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Comonad.INFIX with type 'a t = 'a Core.t
