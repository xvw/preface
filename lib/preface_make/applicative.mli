(** Modules for building {!Preface_specs.APPLICATIVE} modules. *)

(** {1 Tutorial}

    As with [functors], [Applicative] allow multiple paths to build
    [applicative functors].

    {2 Basics}

    Unlike [Functor], [Applicative] offers several primitive ways to be built.
    These different approaches lead to the same combiners. It's up to you to
    choose the most flexible approach according to the context.

    {3 Using [pure] and [apply]}

    This approach is by far the most popular. This is the default one used in
    Haskell. Here's an example with our well-know [Option] module.

    {[
      (* In: option.ml *)
      module Applicative = Preface_make.Applicative.Via_apply (struct
        type 'a t = 'a option

        let pure x = Some x

        let apply fa xa =
          (match (fa, xa) with (Some f, Some x) -> Some (f x) | _ -> None)
        ;;
      end)
    ]}
    {[
      (* In: option.mli *)
      module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a option
    ]}

    Now, your [Option] module is handling an [Applicative] module and you'll be
    able to use features offered by an [Applicative]. For example:

    {[
      let validate age name =
        let open Option.Applicative.Infix in
        (fun age name -> { age; name })
        <$> (fun age -> if age < 0 then None else Some age)
        <*> (fun name -> if String.length name < 3 then None else Some name)
      ;;
    ]}

    {3 Using [pure], [map] and [product]}

    Although this approach is less popular because it requires more code than
    the previous one, it is also possible to express an applicative via the
    [map] and [product] functions.

    {[
      (* In: option.ml *)
      module Applicative = Preface_make.Applicative.Via_map_and_product (struct
        type 'a t = 'a option

        let pure x = Some x

        let map f = function Some x -> Some (f x) | None -> None

        let product ox oy =
          (match (ox, oy) with (Some x, Some y) -> Some (x, y) | _ -> None)
        ;;
      end)
    ]}

    And exactly like the previous tutorial:

    {[
      (* In: option.mli *)
      module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a option
    ]}

    This approach makes it possible to pool work done in [Functor]. For example,
    if our [Option] module already has a [Functor] module, it would be possible
    to implement the [Applicative] in this way:

    {[
      module Applicative = Preface_make.Via_map_and_product (struct
        type 'a t = 'a option

        let pure x = Some x

        let map f x = Functor.map f x

        let product ox oy =
          (match (ox, oy) with (Some x, Some y) -> Some (x, y) | _ -> None)
        ;;
      end)
    ]}

    The crucial point of these two tutorials and despite the fact that they use
    different interfaces, they produce the same module.

    {3 Using a [Monad]}

    Since every [Monads] are an [Applicative Functor], you can produce an
    [Applicative] giving a [Monad]. (The reverse is not possible because some
    [Applicative] are not Monads. for example [Validation]).

    So, let's imagine that we already have a [Monad] module in our [Option]
    module, here's an example how to make an [Applicative] module using a monad:

    {[ module Applicative = Preface_make.Applicative.Via_monad (Monad) ]}

    And exactly like the previous tutorial:

    {[
      (* In: option.mli *)
      module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a option
    ]}

    {2 Advanced}

    Like for [Functor], an [Applicative] can be build step by step, offering
    your own implementation for each component. We just use {!Make} to implement
    the components of our choice. Let's rewrite our [Option.Applicative] in an
    another way:

    First, let's define our [Core] module, the bedrock of the [Applicative].

    {[
      module Core : Preface_specs.Applicative.CORE = struct
        type 'a t = 'a option

        let pure x = Some x

        let map f = function Some x -> Some (f x) | None -> None

        let product ox oy =
          (match (ox, oy) with (Some x, Some y) -> Some (x, y) | None -> None)
        ;;

        let apply fa xa =
          (match (fa, xa) with (Some f, Some x) -> Some (f x) | _ -> None)
        ;;
      end
    ]}

    With [Core], we can use {!Operation} to deal with the combinators of an
    [Applicative]:

    {[ module Operation = Preface_make.Applicative.Operation (Core) ]}

    Now, we can handle infix operators and syntactic shortcut using respectively
    {!Infix} and {!Syntax}:

    {[
      module Infix = Preface_make.Applicative.Infix (Core) (Operation)
      module Syntax = Preface_make.Applicative.Syntax (Core)
    ]}

    Now, we have all required component of our [Applicative], let's putting it
    all together:

    {[
      module Applicative =
        Preface_make.Applicative.Via (Core) (Operation) (Infix) (Syntax)
    ]}

    And as always:

    {[
      (* In: option.mli *)
      module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a option
    ]}

    Exactly like [Functor], [Applicative] can be built using different path, but
    in many cases, the simple approach is recommanded. *)

(** {1 Documentation} *)

(** {2 Construction}

    Standard way to build an [Applicative Functor]. *)

(** Incarnation of an [Applicative] with standard requirements ([pure], [map]
    and [product]). *)
module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative
                                 .CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_with_map_and_product.t

(** Incarnation of an [Applicative] with standard requirements ([pure] and
    [apply]). *)
module Via_apply (Core_with_apply : Preface_specs.Applicative.CORE_WITH_APPLY) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core_with_apply.t

(** Incarnation of an [Applicative] using a [Monad].*)
module From_monad (Monad : Preface_specs.MONAD) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Monad.t

(** Incarnation of an [Applicative] using an [Alternative].*)
module From_alternative (Alternative : Preface_specs.ALTERNATIVE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Alternative.t

(** {2 Manual construction}

    Advanced way to build an [Applicative Functor], constructing and assembling
    a component-by-component an applicative functor. (In order to provide your
    own implementation for some features.) *)

(** Incarnation of an [Applicative] using each components of an [Applicative]. *)
module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Core.t

(** Incarnation of an [Applicative.Core] for an ['a t] with standard
    Requirements ([pure], [map] and [product]). *)
module Core_via_map_and_product
    (Core : Preface_specs.Applicative.CORE_WITH_MAP_AND_PRODUCT) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t

(** Incarnation of an [Applicative.Core] with standard requirements ([pure],
    [apply]). *)
module Core_via_apply (Core : Preface_specs.Applicative.CORE_WITH_APPLY) :
  Preface_specs.Applicative.CORE with type 'a t = 'a Core.t

(** Incarnation of an [Applicative.Operation] with standard requirements
    ([pure], [map], [apply] and [product]). *)
module Operation (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t

(** Incarnation of an [Applicative.Syntax] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Syntax (Core : Preface_specs.Applicative.CORE) :
  Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t

(** Incarnation of an [Applicative.Infix] with standard requirements ([pure],
    [map], [apply] and [product]). *)
module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) :
  Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t
