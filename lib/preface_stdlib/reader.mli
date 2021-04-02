(** A specialized version of a [Reader monad] with [Identity] as the inner
    monad. Since [Preface.Make.Reader] is a Transformer, this module exposes the
    classical [Reader] monad. *)

(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Reader module gives you the ability to read from an environment. This is
    done thanks to the [eval] function which takes the environment and returns a
    value. Finally, this type of environment is given by applying the "Over"
    functor module on a module providing the required type.

    {1 Example}

    In order to illustrate the [Reader] module we design a naive template model.

    {2 Template Representation}

    A template is denoted by a list of template item which can either be a
    string or a variable.

    {[
      type template_item =
        | Const of string
        | Var of string

      type template = template_item list
    ]}

    {2 Reader Construction}

    For the interpration of such naive template data we need an environment used
    to solve variable items.

    {[ module Bindings = Map.Make (String) ]}

    Then building a [Reader] can be done applying the [Over] function on the
    previous [Bindings] module.

    {[
      module Reader = Preface_stdlib.Reader.Over (struct
        type t = string Bindings.t
      end)
    ]}

    {2 Template interpretation}

    Since [Bindings] and [Reader] are available, now we can easily design the
    transformation functions for an item and for a list of items.

    {[
      let transform_item =
        let open Reader in
        let open Bindings in
        function
        | Const s -> return s
        | Var s ->
          let* env = ask in
          return (if mem s env then find s env else "?")
      ;;
    ]}

    In this function, the environment in retrieved thanks to the syntaxic
    extension of monads [let*] and the [ask] method provided by [Reader]. Here,
    it's important to notice that such environment is hold by the [Reader]
    element and can be seen as an implicit value i.e. a kind of dependency
    injection. Finally the transform of a list can be easily design using the
    syntaxic extension of monads [let*].

    {[
      let rec transform =
        let open Reader in
        function
        | [] -> return ""
        | a :: l ->
          let* ta = transform_item a in
          let* tl = transform l in
          return (ta ^ tl)
      ;;
    ]}

    {2 Performing the transformation}

    The last piece of the jigsaw relies on the capability to perform such
    transformation thanks to the [Reader] [run] function.

    {[ let run t env = Reader.eval (transform t) env ]} *)

(** {1 Implementation} *)

module Over (Env : Preface_specs.Types.T0) : sig
  include Preface_specs.READER with type env = Env.t
  (** {2 Monad API} *)

  val eval : 'a t -> Env.t -> 'a
  (** Run the reader through the [identity].*)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)
end

(** {1 Additional Example}

    In this example we propose the transformation of lambda expression to
    DeBruijn indexed lambda expressions.

    {2 Lambda expression ADT}

    A Lambda term can be a variable (free or bound), a function i.e. Abstraction
    and the application of a function on a term.

    {[
      module Lambda = struct
        type t =
          | App of t * t
          | Abs of string * t
          | Var of string
      end
    ]}

    Since beta-reduction costs a lot the traditional approach consists in a term
    transformation to De Bruijn indexed lambda terms

    {2 Indexed lambda expression ADT}

    A Lambda term can be a variable i.e. a position in an environment (direct
    access), a function i.e. Abstraction and the application of a function on a
    term.

    {[
      module DeBruijn = struct
        type t =
          | App of t * t
          | Abs of t
          | Var of int
      end
    ]}

    {2 Lookup}

    The lookup function is a basic function returning the index of an expression
    in a list. The first index is one if we conform the definition given by De
    Bruijn.

    {[
      let lookup n e =
        let rec lookup i = function
          | [] -> None
          | n' :: _ when n' = n -> Some i
          | _ :: e -> lookup (i + 1) e
        in
        lookup 1 e
      ;;
    ]}

    {2 Transformation process}

    The transformation process is now really simple. Given a lambda term we
    propose its transformation thanks to an environement that is a list of
    strings. For this purpose we first create the associate reader:

    {[
      module Reader = Preface_stdlib.Reader.Over (struct
        type t = string list
      end)
    ]}

    In addition, we use the [Try] module for the result. Then is the term as a
    free variable - when lookup returns [None] - it's an error; otherwise we
    return the transformed lambda expression.

    {[
      module Try = Preface_stdlib.Try

      exception FreeVariable of string
    ]}

    The transformation process can now be define:

    {[
      (* transform : Lambda.t -> DeBruijn.t Try.t Reader.t *)
      let rec transform =
        let open Reader in
        function
        | Lambda.App (f, a) ->
          let* tf = transform f in
          let* ta = transform a in
          return
            Try.Applicative.((fun f a -> DeBruijn.App (f, a)) <$> tf <*> ta)
        | Lambda.Abs (n, t) ->
          let* tf = local (fun e -> n :: e) (transform t) in
          return Try.Functor.((fun tf -> DeBruijn.Abs tf) <$> tf)
        | Lambda.Var n -> (
          let* env = ask in
          match lookup n env with
          | None -> return (Error (FreeVariable n))
          | Some i -> return (Ok (DeBruijn.Var i)) )
      ;;
    ]}

    The [Lambda.Abs] transformation is done using the [local] mechanism allowing
    the modification of the environmenent pushing the variable on it. The
    [Lambda.Var] transformation is done using the [ask] mechanism used t
    retrieve the context. *)
