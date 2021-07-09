(** The library is divided into 4 parts (in the user area) which serve
    complementary purposes.*)

(** {%html:
<table border="0" cellspacing="0" style="width: 100%;">
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;">Preface.Specs</strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      Contains all the interfaces of the available abstractions.
      The specifications resemble the <code>_intf</code> suffixed
      signatures found in other libraries in the OCaml ecosystem.
    </td>
  </tr>
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;">Preface.Make</strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      Contains the set of <i>functors</i> (in the ML sense of the term)
      for concretising abstractions. Schematically, a module in
      <code>Preface.Make</code> takes a module (or modules) respecting a
      signature described in <code>Preface.Specs</code> to produce a
      complete signature (also described in <code>Preface.Specs</code>).
    </td>
  </tr>
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;">Preface</strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      Contains concrete implementations, constructs that implement
      abstractions described in <code>Preface.Specs</code> by means of
      the functors present in <code>Preface.Make</code>.
      This library is, at least, an example of the use of
      <code>Specs</code> and <code>Make</code>.
    </td>
  </tr>
 <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;">Preface.Laws</strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      Functors to generate laws for a given abstraction.
    </td>
  </tr>
</table>
%} *)

(** {1 Abstraction implementations}

    Functor (in Haskell sense), Applicatives and monads are some of the best
    known abstractions in functional programming. Indeed, they allow recurrent
    problems to be solved in an elegant way. Generally, thanks to certain
    mechanisms linked to the languages that implement them (in Haskell, for
    example, using typeclasses), it is possible, by defining only a small subset
    of their combinators, to derive many others. So the purpose of "this part of
    the library" is to provide mechanisms for deriving combinators for a given
    type and a chosen abstraction, respecting OCaml programming idioms as much
    as possible. *)

(** {2 Specifications}

    This module describes the specifications of the abstractions provided by
    [Preface]. These specifications, which correspond to interfaces
    ([module types] in OCaml terminology) serve as constraints for the
    [functors] described in [Preface.Make] and centralise the documentation.
    Using a separate module allows cyclic dependencies to be resolved if one
    module can be described by another module and {e vice versa}. *)

module Specs = Preface_specs

(** {2 Achievements}

    In order to produce embodiments for the abstractions described in
    [Preface.Specs], [Preface.Make] offers a collection of functors that take
    modules constrained by the interfaces described in [Preface.Specs] to
    produce modules that respect the more complete interfaces also described in
    [Preface.Specs]. *)

module Make = Preface_make

(** {2 Concepts, Naming and Terminology}

    The modular design of Preface may seem a little intimidating at first
    glance. Let's look at the logic of the cut to understand how best to use it
    to describe new achievements of abstractions.

    Abstractions must respect a minimum interface, however, sometimes there are
    several paths to describe the abstraction. For example, building a [Monad]
    on a type requires a [return] (or [pure] depending on the convention in
    practice) and:

    - [bind]/[>>=]
    - or [map] and [join]
    - or sometimes [>=>]

    In addition, on the basis of these minimum combinators, it is possible to
    derive other combinators. However, it happens that these combinators are not
    implemented in an optimal way (this is the cost of abstraction). In the
    OCaml ecosystem, the use of polymorphic variants is sometimes used to give
    the user the freedom to implement, or not, a function by wrapping the
    function definition in a value of this type:

    {[
      val f : [< `Derived | `Custom of 'a -> 'b ]
    ]}

    Instead of relying on this kind of (rather clever!) trick, we decided to
    rely mainly on the module language.

    To make it easy to describe the embodiment of an abstraction, but still
    allow for the possibility of providing more efficient implementations (that
    propagate new implementations on aliases, such as infix operators, or
    functions that use these functions), Preface proposes a rather particular
    cut.

    Each abstraction is broken down into several sub-modules: *)

(** {%html:
<table border="0" cellspacing="0" style="width: 100%;">
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;"><code>Core</code></strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
        This module describes all the fundamental operations. For example,
       for a monad, we would find <code>return<code>, <code>map</code>,
       <code>bind</code>, <code>join</code> and
       <code>compose_left_to_right</code>
    </td>
  </tr>
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;"><code>Operation</code></strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
        The module contains the set of operations that can be described
        using the <code>Core</code> functions.
    </td>
  </tr>
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;"><code>Infix</code></strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
        The module contains infix operators built on top of the
        <code>Core</code> and <code>Operation</code>.
    </td>
  </tr>
  <tr>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
      <strong style="font-weight: 900;"><code>Syntax</code></strong>
    </td>
    <td style="border:1px solid #d5d5d5; padding: 6px;">
        The module contains the <code>let</code> operators (such as
        <code>let*</code> and <code>let+</code> for example), built with
        the <code>Core</code> and <code>Operation</code> functions.
    </td>
  </tr>
</table>
%} *)

(** {e Sometimes it happens that some modules are not present (e.g. when there
       are no infix operators) or sometimes some additional modules are added,
       but in general the documentation is clear enough.}

    The functors exposed in [Preface.Make] allow you to build each component one
    by one ([Core], [Operation], using [Core], and [Infix] and [Syntax] using
    [Core] and [Operation]) and then group all these modules together to form
    the abstraction. Or use the {e Happy Path}, which generally offers a similar
    approach to functors which builds [Core] but builds the whole abstraction.

    {%html:
      <center>
        <img
          style="width:75%; margin: 8%;"
          src="https://ocaml-preface.github.io/images/cut.svg"
          alt="Module cutting"
        >
      </center> %} *)

(** Although it is likely that the use of the {e Happy Path} covers a very large
    part of the use cases and that it is not necessary to achieve every
    abstraction by hand, it is still possible to do so.

    In addition, it is sometimes possible to describe one abstraction by
    specialising another. In general, these specialisations follow this naming
    convention: [From_name (More_general_module)] or
    [To_name (Less_general_module)] and sometimes you can build a module on top
    of another, for example Selective on top of Applicative and the naming
    follows this convention: [Over_name (Req)], ie:
    [Selective.Over_applicative]. *)

(** {1 Standard library}

    Whereas the previous section dealt mainly with the achievements of
    abstractions (using functor machinery). This section documents the standard
    Preface library. A collection of already implemented abstractions for
    relatively common data structures. *)

include Preface_stdlib
(** @inline *)

(** {1 Laws}

    Many of the abstractions presented in [Preface] are governed by laws, to
    ensure the proper functioning of other derived operations. This library
    provides functors to generate implemented laws for a concretization of an
    abstraction. *)

module Laws = Preface_laws
