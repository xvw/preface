```ocaml
# #require "preface" ;;
```

# Understanding the breakdown of Preface modules by instantiating monad for Option

As mentioned
[here](https://github.com/xvw/preface#some-design-choices), Preface
uses a module cut that may seem unusual. Indeed, `Preface.Make`
generally offers several approaches to building a concrete version of
an abstraction. These different ways can either be different paths to
build a concrete implementation (for example, provide the full
`Monad` API for the `Option` type, which we will use as an example in
this guide), or build concrete fragments (for example, build only
infix operators) of a more general API, or provide mechanisms for
transformations between different modules (for example, the
composition of two functors).

Although several implementations for the `'a option` type are already
described in the standard Preface library, we will look at several
ways of providing a `Monad` module for the `'a option` type.

## Module dissection

In many cases, it is sufficient to provide a minimal set of functions
to build a complete API. In this guide, we will refer to this as a
"Happy Path".

In many cases the abstractions proposed by Preface are built on the
basis of these modules (there are exceptions. For example some
abstractions are more complex, others less so).

- `Core`: which contains the fundamental operations.
- `Operation`: which contains the operations that can be derived from
  the `Core` operations.
- `Infix`: which contains infix operators.
- `Syntax`: which contains `let-operators`.

And in general, the overall shape of the module looks like this:

```ocaml non-deterministic=command
module S = struct 
  include Core
  include Operation with type t := t
  module Infix with type t := t
  module Syntax with type t := t
  include Infix
  include Syntax
end
```

As a general rule, `Preface.Make` provides functors to build each
component independently, a functor to generalise the inclusion logic
presented above, and functors that for minimal interfaces (generally,
these minimal interfaces are the same as one would give to the functor
that builds the `Core` because from `Core`, it is possible to build
everything else. The `Operation` functor usually takes `Core` as an
argument, with `Infix` and `Syntax` taking `Core` and `Operation`.)
produce the whole API.


## Provide a Monad module for Option using the Happy Path

Our goal is to build the entire API of a monad on the Option type. As
mentioned in the
[documentation](https://ocaml-preface.github.io/preface/Preface_specs/Monad/index.html#minimal-definition),
it is possible to use 3 minimal interfaces:

- [WITH_BIND](https://ocaml-preface.github.io/preface/Preface_specs/Monad/module-type-WITH_BIND/index.html)
  which requires the implementation of `return` and `bind`
- [WITH_MAP_AND_JOIN](https://ocaml-preface.github.io/preface/Preface_specs/Monad/module-type-WITH_MAP_AND_JOIN/index.html)
  which requires the implementation of `return`, `map` and `join`
- [WITH_KLEISLI_COMPOSITION](https://ocaml-preface.github.io/preface/Preface_specs/Monad/module-type-WITH_KLEISLI_COMPOSITION/index.html)
  which requires the implementation of `return` and `compose_left_to_right`

### Using Bind

Using `bind` is a fairly popular method because generally it is a path
that is fairly well understood. So far so good, let's implement a
monad on option via the `bind` function!

```ocaml
module Opt_monad = Preface.Make.Monad.Via_return_and_bind (struct 
  type 'a t = 'a option 
  
  let return x = Some x
  
  let bind f = function 
    | Some x -> f x 
    | None -> None
end)
```

And ... that's it. We can try to use our newly created monad!

```ocaml
# let result = let open Opt_monad in
     return 10       >>= fun x -> 
     return (x + 15) >>= fun y -> 
     return (y + 30) ;;
val result : int option = Some 55
```

Or we can use the `let operator` for an even better result!

```ocaml
# let result = let open Opt_monad in 
    let* x = return 10 in 
    let* y = return (x + 15) in 
    return (y + 30) ;;
val result : int option = Some 55
```

### Using Map and Join

Although implementations that rely on `bind` are more common, one
could also describe a complete interface using `map` and `join` (which
is conceptually closer to the categorical definition of a monad):

```ocaml
module Opt_monad_2 = Preface.Make.Monad.Via_return_map_and_join (struct 
  type 'a t = 'a option 
  
  let return x = Some x
  
  let map f = function 
    | Some x -> Some (f x)
    | None -> None
    
  let join = function 
    | Some x -> x 
    | None -> None
end)
```

And we can retest the code we wrote earlier, but with our new module:

```ocaml
# let result = let open Opt_monad_2 in
     return 10       >>= fun x -> 
     return (x + 15) >>= fun y -> 
     return (y + 30) ;;
val result : int option = Some 55
```

## Using a manual approach

Although the Happy Path is generally sufficient, sometimes the derived
definitions (mainly for `Core` and `Operation`, as the operators and
syntax are generally just function calls from the previous two
modules) are not efficient enough (the cost of Abstraction!).  In the
Haskell documentation, one can often read "*The default definition is
``a definition`, but this may be overridden with a more efficient
version.*" 

> It is partly for this reason that it was decided to divide the
> Preface modules in this way. Typically, we thought, no worries if a
> user wants to change the implementation of a function to a "more
> efficient" one, they can just do something like this (this is an
> artificial example):

```ocaml non-deterministic=command
module My_monad_with_extra_efficient_map = struct 
  include Preface.Make.Monad.Via_bind (struct 
     type 'a t = 'a option 
     let bind f = function None -> None | Some x -> f x
  end)
  let map f x = Super_powerful_module.efficient_option_map f x
end
```

> It would seem to work, but... the very efficient implementation of
> map would not be propagated to aliases of map (e.g.. its infix
> version) and all functions that use `map` (e.g. in operations) would
> use the old version which, in turn... would not be as efficient (as
> its name seems to indicate).

That's why Preface allows you to build a module with many
[intermediate
functors](https://ocaml-preface.github.io/preface/Preface_make/Monad/index.html#manual-construction)!

Well, first, let's implement the core of our module, Let's say I
implement it with bind but provide a very fast implementation of
`map`:

```ocaml
module Opt_core = struct
  include Preface.Make.Monad.Core_via_return_and_bind (struct 
    type 'a t = 'a option 
    let return x = Some x
    let bind f = function Some x -> f x | None -> None
  end)
  (** And now, I can give my magical-map-implementation *)
  
  let map f x = 
    (* Damn, why!!! Don't worry, it is only as a Proof
       that it was this map which is used. *)
    let () = print_endline "Yeah, Im a Magic Map" in 
    match x with Some y -> Some (f y) | None -> None
    
end
```

Now we can build our modules one by one! And as with `Core`, we will
use functors capable of building fragments:

```ocaml
module Opt_op = Preface.Make.Monad.Operation (Opt_core)
module Opt_infx = Preface.Make.Monad.Infix (Opt_core) (Opt_op)
module Opt_syn = Preface.Make.Monad.Syntax (Opt_core)
```

Now that we have all our modules, let's group them together using the
[Via
functor](https://ocaml-preface.github.io/preface/Preface_make/Monad/Via/index.html)!

```ocaml
module Opt_with_magic_map = 
   Preface.Make.Monad.Via 
       (Opt_core) (Opt_op) (Opt_infx) (Opt_syn)
```

And has our horrible addition spread to the `map` aliases? Here is a
use of a lot of infix operators that all do the same thing! If the
magic aspect of our `map` has propagated correctly, we should see 4
messages (each time the same) on the standard output!

```ocaml
# Opt_with_magic_map.(succ =|< return 10) ;;
Yeah, Im a Magic Map
- : int option = Some 11
# Opt_with_magic_map.(return 11 >|= succ) ;;
Yeah, Im a Magic Map
- : int option = Some 12
# Opt_with_magic_map.(succ <$> return 12) ;;
Yeah, Im a Magic Map
- : int option = Some 13
# Opt_with_magic_map.(return 13 <&> succ) ;;
Yeah, Im a Magic Map
- : int option = Some 14
```

And voila!

## Conclusion

Even if very often the Happy Path is largely sufficient, in more
advanced cases it is possible not to rely on the automatic function
derivation. On the other hand, as there are functors for each fragment
of a module, it is possible to use them "*à la carte*" so that in the
case where only one function needs to be reimplemented... everything
has to be written by hand! I hope this guide has helped you to
understand how to use Preface and to be able to justify certain design
choices!
