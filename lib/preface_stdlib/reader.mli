(** {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The Reader module gives you the ability to read from an environment. This is
    done thanks to the `run` function which takes the environment and returns a
    value. Finally, this type of environment is given by applying the "Over"
    functor module on a module providing the required type.

    {1 Example}

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
    strings.

    For this purpose we first create the associate reader:

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
        let open Reader.Monad in
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

(** {1 Implementation} *)
module Over (T : Preface_specs.Types.T0) : sig
  (** {2 Types} *)

  type env = T.t
  (** The encapsulated state *)

  type 'a t = env -> 'a
  (** The type *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)

  (** {2 Helpers} *)

  val run : 'a t -> env -> 'a
  (** Run the reader and extracting the value *)

  val ask : env t
  (** Provides the monad environment *)

  val local : (env -> env) -> 'a t -> 'a t
  (** Modify the environement and execute the reader *)

  val reader : (env -> 'a) -> 'a t
  (**  *)
end
