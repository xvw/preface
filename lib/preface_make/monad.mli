(** Modules for building {!Preface_specs.MONAD} modules. *)

(** {1 Tutorial}

    As with [Functor] and [Applicative], a [Monad] allow multiple path to build
    a [Monad] module.

    {2 Basics}

    As [Applicative], [Monad] offer several primitive ways to be built. These
    different approaches lead to the same combiners. It's up to you to choose
    the most flexible approach according to the context.

    {3 Using [bind]}

    This approach is by far the most popular. This is the default one used in
    Haskell. Here's an example with our well-know [Option] module.

    {[
      (* In: option.ml *)
      module Monad = Preface_make.Monad.Via_bind (struct
        type 'a t = 'a option

        let return x = Some x

        let bind f = function Some x -> f x | None -> None
      end)
    ]}
    {[
      (* In: option.mli *)
      module Monad : Preface_specs.MONAD with type 'a t = 'a option
    ]}

    Now, your [Option] module is handling a [Monad] module and you'll be be able
    to use features offered by a [Monad]. For example:

    {[
      let operation =
        let open Option.Monad.Infix in
        return 45 >>= (fun x -> Some (x + 10)) >|= (fun x -> x + 20)
      ;;
    ]}

    {3 Using [map] and [join]}

    You can express a Monad using [map] and [join]. For example, let's try to
    implement a [List] monad:

    {[
      (* In: list.ml *)
      module Monad = Preface_make.Monad.Via_map_and_join (struct
        type 'a t = 'a option

        let return x = [ x ]

        let map = Stdlib.List.map

        let join = Stdlib.List.concat
      end)
    ]}
    {[
      (* In: list.mli *)
      module Monad : Preface_specs.MONAD with type 'a t = 'a list
    ]}

    {2 Advanced}

    As you can see in the documentation, you can express a monad using the
    [Kleisli composition] and exactly like [Functor] and [Applicative], you can
    build a monad step by step, offering your own implementation for each
    component. *)

(** {1 Documentation} *)

(** {1 Construction}

    Standard way to build a [Monad]. *)

(** Incarnation of a [Monad] with standard requirement ([return] and [bind]). *)
module Via_bind (Core_with_bind : Preface_specs.Monad.CORE_WITH_BIND) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_bind.t

(** Incarnation of a [Monad] with map and join requirement ([return], [join] and
    [map]). *)
module Via_map_and_join
    (Core_with_map_and_join : Preface_specs.Monad.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_map_and_join.t

(** Incarnation of a [Monad] with kleisli composition requirement ([return] and
    [compose_left_to_right]). *)
module Via_kleisli_composition
    (Core_with_kleisli_composition : Preface_specs.Monad
                                     .CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.MONAD with type 'a t = 'a Core_with_kleisli_composition.t

(** Incarnation of a [Monad] using a [Monad_plus].*)
module From_monad_plus (Monad_plus : Preface_specs.MONAD_PLUS) :
  Preface_specs.MONAD with type 'a t = 'a Monad_plus.t

(** {2 Manual construction}

    Advanced way to build a [Monad], constructing and assembling a
    component-by-component a monad. (In order to provide your own implementation
    for some features.) *)

(** Incarnation of a [Monad] using each components of a [Monad].*)
module Via
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Monad.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.MONAD with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Core] with standard requirement ([return] and
    [bind]). *)
module Core_via_bind (Core : Preface_specs.Monad.CORE_WITH_BIND) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Core] with map and join requirement ([return],
    [join] and [map]). *)
module Core_via_map_and_join (Core : Preface_specs.Monad.CORE_WITH_MAP_AND_JOIN) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Core] with kleisli composition requirement ([return]
    and [compose_left_to_right]). *)
module Core_via_kleisli_composition
    (Core : Preface_specs.Monad.CORE_WITH_KLEISLI_COMPOSITION) :
  Preface_specs.Monad.CORE with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Operation] with [Monad.Core].*)
module Operation (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Syntax] with [Monad.Core].*)
module Syntax (Core : Preface_specs.Monad.CORE) :
  Preface_specs.Monad.SYNTAX with type 'a t = 'a Core.t

(** Incarnation of a [Monad.Infix] with [Monad.Core] and [Monad.OPERATION].*)
module Infix
    (Core : Preface_specs.Monad.CORE)
    (Operation : Preface_specs.Monad.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Monad.INFIX with type 'a t = 'a Core.t
