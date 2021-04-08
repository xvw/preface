```ocaml
# #require "preface"
```

# A deep dive into the modular breakdown of Preface

Preface uses a modular cut that can be complex at first glance. In
this guide, we will see how to implement several abstractions, in
several ways, for the `option` type. Preface's design choice are
briefly explained/described [on the
README](https://github.com/xvw/preface#some-design-choices)

## Giving a Functor to our Option

Although cutting is daunting, Preface tries as much as possible to
offer an "easy" approach, which we will call the "*Happy Path*". For
example, let's add a `Functor` for `option`.  The most standard
approach to building a Functor is to provide a parameterised type and
a map function. So, we could use the module
`Preface.Make.Functor.Via_map` and give it a module of type:
`Preface.Specs.Functor.WITH_MAP` respecting this interface:

```ocaml
# #show Preface.Specs.Functor.WITH_MAP
module type WITH_MAP = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
```

Now that we know how to implement a `Functor`, let's do it.

```ocaml
module Opt_functor = Preface.Make.Functor.Via_map (struct
  type 'a t = 'a option 
  let map f = function 
    | Some x -> Some (f x) 
    | None -> None
end) 
```

Let's see if the whole interface has been implemented:

```ocaml
# #show Opt_functor
module Opt_functor :
  sig
    type 'a t = 'a option
    val map : ('a -> 'b) -> 'a option -> 'b option
    val replace : 'a -> 'b option -> 'a option
    val void : 'a option -> unit option
    module Infix : sig ... end
    val ( <$> ) : ('a -> 'b) -> 'a option -> 'b option
    val ( <&> ) : 'a option -> ('a -> 'b) -> 'b option
    val ( <$ ) : 'a -> 'b option -> 'a option
    val ( $> ) : 'a option -> 'b -> 'b option
  end
```

Perfect. Now let's try to use our freshly created combinators.

```ocaml
# Opt_functor.(succ <$> Some 10)
- : int option = Some 11
```

For many objects, it is generally not necessary to go further than the
minimum definition. It allows to derive a complete interface by
implementing relatively few functions (just the bare necessities).

## Giving a monad to our option

`Functor` is a special case because it offers only one definition
path. On the other hand, `Monad` is a bit more interesting, as it is
possible to describe a monad in three different ways:

1. Using `return` and `bind`
2. Using `return`, `map` and `join`
3. Using `return`, and the kleisli composition.

For this tutorial, we will see how to build a `Monad` module using the
first two strategies.

### Using Bind

This is generally the most standard way to describe a monad (at least
in the Haskell world). First Let's see what requirements are needed to
build a monad with `bind` (and `return`):

```ocaml
# #show Preface.Specs.Monad.WITH_BIND
module type WITH_BIND =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
  end
```

> You may have noticed that the signature is called `WITH_BIND` and
> not `WITH_RETURN_AND_BIND`. This is an internal Preface
> convention. When a function must be provided in all minimal
> definitions, it is mandatory and therefore not repeated in its
> module name.



