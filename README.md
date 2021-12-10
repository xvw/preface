# Preface

> Preface is an opinionated library designed to facilitate the
> handling of recurring functional programming idioms in
> [OCaml](https://ocaml.org). Many of the design decisions were made
> in an attempt to calibrate, as best as possible, to the OCaml
> language. Trying to get the most out of the module language. _The
> name "preface" is a nod to "Prelude"_.

When learning functional programming, one is often confronted with
constructs derived (or not) from category theory. Languages such as
Haskell offer very complete libraries to use them, and thus,
facilitate their learning. In OCaml, it often happens that these
abstractions are buried in the heart of certain libraries/projects
([Lwt](https://ocsigen.org/lwt/latest/manual/manual),
[Cmdliner](https://erratique.ch/logiciel/cmdliner),
[Bonsai](https://github.com/janestreet/bonsai),
[Dune](https://dune.build) etc.). This is why one of the objectives of
Preface is to propose tools for concretising these abstractions, at
least as a pedagogical tool.

### Is Preface useful

Since OCaml allows for efficient imperative programming, Preface is
probably not really useful for building software. However, we (the
maintainers) think that Preface can be useful for a few things:

- technical experimentation with abstractions (especially those from
  the Haskell world) that allow programming in a fun style.
- As an educational tool. Many teaching aids generally only offer the
  minimal interfaces to these abstractions. Preface tries to be as
  complete as possible.
- It was a lot of fun to make. The last point is obviously the
  lightest but building Preface was really fun! So even if some people
  won't see the point... **we had fun making it**!

## Installation

The package is available on OPAM, `opam install preface` should be enough. (And
by describing, of course, OPAM in your project OPAM file and linking it to your
project in the standard way proposed by your build-system.)

### OPAM pin

If you would like to use the latest version of Preface (under development) you
can use the [pin mechanism](https://opam.ocaml.org/doc/Usage.html#opam-pin).

```
...
depends: [
  ...
  "preface" {pinned}
]

pin-depends: [
  ["preface.dev" "git+ssh://git@github.com/xvw/preface.git"]
  ...
]
...
```

### Esy resolution

The library can also be installed with [esy](esy.sh) using a resolution in your `package.json` file :

```json
...
    "dependencies": {
      ...
      "@opam/preface":"*"
    },
    "resolutions": {
        "@opam/preface":"xvw/preface#<commit>"
    },
...
```

The pattern of the resolution is `xvw/preface#<commit>` where `<commit>` is mandatory and [should point to a specific commit](https://esy.sh/docs/en/using-repo-sources-workflow.html#with-esy-packages).

## Library anatomy

The library is divided into four parts (in the user area) which serve
complementary purposes.

| Library          | Description                                                                                                                                                                                                                                                                                |
| ---------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `preface.specs`  | Contains all the interfaces of the available abstractions. The specifications resemble the `_intf` suffixed signatures found in other libraries in the OCaml ecosystem.                                                                                                                    |
| `preface.make`   | Contains the set of _functors_ (in the ML sense of the term) for concretising abstractions. Schematically, a module in `Preface.Make` takes a module (or modules) respecting a signature described in `Preface.Specs` to produce a complete signature (also described in `Preface.Specs`). |
| `preface.stdlib` | Contains concrete implementations, constructs that implement abstractions described in `Preface.Specs` by means of the functors present in `Preface.Make`. This library is, at least, an example of the use of `Specs` and `Make`.                                                         |
| `preface`        | Packs all libraries making `Preface.Specs` and `Preface.Make` accessible as soon as Preface is available in the current scope. And includes `Preface.Stdlib` (so everything in `Preface.Stdlib` is available from Preface).                                                                |

## Available abstractions in `Make` and `Specs`

Although `Stdlib` offers common and, in our view, useful
implementations, the heart of Preface lies in its ability to build the
concretisation of abstractions for all sorts of data structures. Here
is a list of abstractions that can be built relatively easily. As you
can see, the diagram is heavily inspired by the
[Haskell](https://haskell.org) community's
[Typeclassopedia](https://wiki.haskell.org/Typeclassopedia).

<p align="center"><img src="https://ocaml-preface.github.io/images/specs.svg" alt="typeclassopedia"></p>

Obviously, the set of useful abstractions is still far from being present in
Preface. We have decided to privilege those for which we had a short and medium
term use. But if you find that an abstraction is missing, the development of
Preface is open, don't hesitate to contribute by adding what was missing.

## Concretisation in `Stdlib`

As for the implemented abstractions, we favoured objects that we often
manipulated (that we constantly reproduced in our projects) and also
those that allowed us to test certain abstractions (`Predicate` and
`Contravariant` for example). Don't hesitate to add some that would be
useful for the greatest number of people!

| Name                  | Description                                                                       | Abstractions                                                                                                                                                                                         |
| --------------------- | --------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Approximation.Over`  | A generalization of `Const` (the phantom monoid) for over approximation           | `Applicative`, `Selective`                                                                                                                                                                           |
| `Approximation.Under` | Same of `Over` but for under approximation                                        | `Applicative`, `Selective`                                                                                                                                                                           |
| `Continuation`        | A continuation that can't be delimited                                            | `Functor` (and `Invariant`), `Applicative`, `Monad`                                                                                                                                                  |
| `Env`                 | The env comonad using `Identity`as inner monad                                    | `Functor` (and `Invariant`), `Comonad`                                                                                                                                                               |
| `Either`              | Represent a disjunction between `left` and `right`                                | `Bifunctor` and can be specialised for the `left` part; `Functor` (and `Invariant`), `Alt`, `Applicative`, `Monad`, `Traversable` through Applicative and Monad, `Foldable`                          |
| `Fun`                 | Function `'a -> 'b`                                                               | `Profunctor`, `Strong`, `Choice`, `Closed`, `Semigroupoid`, `Category`, `Arrow`, `Arrow_choice`, `Arrow_apply`                                                                                       |
| `Identity`            | A trivial type constructor, `type 'a t = 'a`                                      | `Functor` (and `Invariant`), `Applicative`, `Selective`, `Monad`, `Comonad`                                                                                                                          |
| `List`                | The standard list of OCaml                                                        | `Foldable`, `Functor` (and `Invariant`), `Applicative`, `Alternative`, `Selective`, `Monad`, `Monad_plus`, `Traversable` through Applicative or Monad, `Monoid` (where the inner type must be fixed) |
| `Nonempty_list`       | A list with, at least, one element                                                | `Foldable`, `Functor` (and `Invariant`), `Alt`, `Applicative`, `Selective`, `Monad`, `Comonad`, `Traversable` through Applicative or Monad, `Semigroup` (where the inner type must be fixed)         |
| `Option`              | Deal with absence of values                                                       | `Foldable`, `Functor` (and `Invariant`), `Applicative`, `Alternative`, `Monad`, `Monad_plus`, `Traversable` through Applicative of Monad, `Monoid` (where the inner type must be fixed)              |
| `Predicate`           | A generalization of function `'a -> bool`                                         | `Contravariant` (and `Invariant`), `Divisible`, `Decidable`                                                                                                                                                       |
| `Reader`              | The reader monad using `Identity` as inner monad                                  | `Functor` (and `Invariant`), `Applicative`, `Monad`                                                                                                                                                  |
| `Result`              | Deal with `Ok` or `Error` values                                                  | `Bifunctor` and can be specialised for the `error` part; `Functor` (and `Invariant`), `Alt`, `Applicative`, `Monad`, `Traversable` through Applicative and Monad, `Foldable`                         |
| `Seq`                 | The standard sequence of OCaml                                                    | `Foldable`, `Functor` (and `Invariant`), `Applicative`, `Alternative`, `Selective`, `Monad`, `Monad_plus`, `Traversable` through Applicative or Monad, `Monoid` (where the inner type must be fixed) |
| `State`               | The state monad using `Identity` as inner monad                                   | `Functor` (and `Invariant`), `Applicative`, `Monad`                                                                                                                                                  |
| `Store`               | The store comonad using `Identity`as inner monad                                  | `Functor` (and `Invariant`), `Comonad`                                                                                                                                                               |
| `Stream`              | Infinite list                                                                     | `Functor` (and `Invariant`), `Applicative`, `Monad`, `Comonad`                                                                                                                                       |
| `Traced`              | The traced comonad using `Identity`as inner monad                                 | `Functor` (and `Invariant`), `Comonad`                                                                                                                                                               |
| `Try`                 | A biased version of `Result` with `exception` as the error part                   | `Functor` (and `Invariant`), `Alt`, `Applicative`, `Monad`, `Traversable` through Applicative and Monad, `Foldable`                                                                                  |
| `Pair`                | A pair `'a * 'b`                                                                  | `Bifunctor`                                                                                                                                                                                          |
| `Validate`            | A biased version of `Validation` with `exception Nonempty_list` as `invalid` part | `Functor` (and `Invariant`), `Alt`, `Applicative`, `Selective`, `Monad`, `Traversable` through Applicative and Monad, `Foldable`                                                                     |
| `Validation`          | Like `Result` but the `invalid` part is a `Semigroup` for accumulating errors     | `Bifunctor` and can be specialized on the `invalid` part: `Functor` (and `Invariant`), `Alt`, `Applicative`, `Selective`, `Monad`, `Traversable` through Applicative and Monad, `Foldable`           |
| `Writer`              | The writer monad using `Identity` as inner monad                                  | `Functor` (and `Invariant`), `Applicative`, `Monad`                                                                                                                                                  |

### Stdlib convention

As it is possible to take several paths to realise an abstraction, we
decided to describe each abstraction in a dedicated sub-module. For
example `Option.Functor` or `Option.Monad` to let the user choose
which combinators to use.

#### Do not shadow the standard library

Although it was tempting to extend the standard OCaml library with
this technique:

```ocaml
module Preface : sig
  module List : sig
    include module type of List
    include module type of Preface_stdlib.List
  end
end
```

We have decided not to do this to ensure consistent documentation (not
varying according to the version of OCaml one is using).

## Some design choices

Abstractions must respect a minimum interface, however, sometimes
there are several paths to describe the abstraction. For example,
building a monad on a type requires a `return` (or `pure` depending on
the convention in practice) and:

- `bind` (`>>=`)
- `map` and `join`
- or possibly `>=>`

In addition, on the basis of these minimum combinators, it is possible
to derive other combinators. However, it happens that these
combinators are not implemented in an optimal way (this is the cost of
abstraction). In the OCaml ecosystem, the use of polymorphic variants
is sometimes used to give the user the freedom to implement, or not, a
function by wrapping the function definition in a value of this type:

```ocaml
val f : [< `Derived | `Custom of ('a -> 'b)]
```

Instead of relying on this kind of (rather clever!) trick, we decided
to rely mainly on the module language.

To make it easy to describe the embodiment of an abstraction, but
still allow for the possibility of providing more efficient
implementations (that propagate new implementations on aliases, such
as infix operators, or functions that use these functions), Preface
proposes a rather particular cut.

Each abstraction is broken down into several sub-modules:

| Submodule   | Role                                                                                                                                                      |
| ----------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Core`      | This module describes all the fundamental operations. For example, for a monad, we would find `return`, `map`, `bind`, `join` and `compose_left_to_right` |
| `Operation` | The module contains the set of operations that can be described using the `Core` functions.                                                               |
| `Infix`     | The module contains infix operators built on top of the `Core` and `Operation`.                                                                           |
| `Syntax`    | The module contains the `let` operators (such as `let*` and `let+` for example), built with the `Core` and `Operation` functions.                         |

> Sometimes it happens that some modules are not present (e.g. when
> there are no infix operators) or sometimes some additional modules
> are added, but in general the documentation is clear enough.

The functors exposed in `Make` allow you to build each component one
by one (`Core`, `Operation`, using `Core`, and `Infix` and `Syntax`
using `Core` and `Operation`) and then group all these modules
together to form the abstraction. Or use the Happy Path, which
generally offers a similar approach to functors which builds `Core`
but builds the whole abstraction.

Here is an example of the canonical flow of concretisation of an
abstraction:

<p align="center"><img src="https://ocaml-preface.github.io/images/cut.svg" alt="module hierarchy"></p>

Although it is likely that the use of the _Happy Path_ covers a very
large part of the use cases and that it is not necessary to concretise
every abstraction by hand, it is still possible to do so.

In addition, it is sometimes possible to describe one abstraction by
specialising another. In general, these specialisations follow this
naming convention: `From_name (More_general_module)` or `To_name (Less_general_module)` and sometimes you can build a module on top of
another, for example `Selective` on top of `Applicative` and the
naming follows this convention: `Over_name (Req)`, ie
`Selective.Over_applicative`

## Projects using Preface

| Project name | Description                                                                                                                  | Links                                                   |
| ------------ | ---------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| **YOCaml**   | YOCaml is a static blog generator that essentially takes advantage of Preface's `Freer`, `Result`, `Validation` and `Arrow`. | [Github repository](https://github.com/xhtmlboi/yocaml) |

You use Preface for one of your projects and you want to be in this
list? Don't hesitate to open a PR or fill an issue, we'd love to hear
from you.

## closing remarks

Preface is a fun project to develop and we have learned a lot from
it. We hope you find it useful and/or enjoyable to use. We are open to
any improvements and open to external contributions!

We received a lot of help during the development of Preface. Feel free
to go to the [CREDITS](CREDITS.md) page to learn more.
