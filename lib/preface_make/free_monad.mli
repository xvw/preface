(** Modules for building {!Preface_specs.FREE_MONAD} modules. *)

(** {1 Tutorial}

    In order to be modular, [Preface] offers multiple way to build a
    {!Preface_specs.FREE_MONAD}. In many case, you just have to use the
    parametrized module {!Over_functor}, but in some particular cases, you want
    to be able to create it from an [Applicative] or another [Monad]

    {2 Basics}

    The most common way to build a [Free_monad] is to use the module
    {!Over_functor}.

    {2 A complete example}

    In this example we propose a Store where strings can be stored and retrieved
    with a key which is also a string.

    {3 Defining a [Functor]}

    The first piece of this jigsaw should be an Algebraic Data Type (ADT) and
    it's dedicated [map] function. In a language like Haskell such mapping
    function can be automatically derived. In [Preface] this is not the case
    yet.

    {[
      (* file: store.ml *)
      module Store = struct
        type 'a t =
          | Get of (string * (string option -> 'a))
          | Set of (string * string * (unit -> 'a))

        module Functor = Preface_make.Functor.Via_map (struct
          type nonrec 'a t = 'a t

          let map f x =
            match x with
            | Get (k, r) -> Get (k, (fun s -> f (r s)))
            | Set (k, v, r) -> Set (k, v, (fun () -> f (r ())))
          ;;
        end)
      end
    ]}

    {3 Creating the [Free_monad]}

    Thanks to the [Preface] library the corresponding [Free_monad] you be simply
    created using the parametric module `Over_functor`.

    {[
      module Store_free = Preface_make.Free_monad.Over_functor (Store.Functor)
    ]}

    {3 Defining an interpeter}

    Then we can propose one interpretation using an OCaml side effect for
    instance, here the side effect is a mutable reference

    {[
      let runStore l = function
        | Store.Get (k, f) ->
          f (Option.map snd (List.find_opt (fun (k', _) -> k' = k) !l))
        | Store.Set (k, v, f) ->
          let () = l := (k, v) :: !l in
          f ()
      ;;
    ]}

    Now we propose two operations i.e. [set] and [get]. This operations are
    reified thanks to the ADT definition. Reification here means a [set] (resp.
    [get]) operation is denoted by the constructor [Set] (resp. [Get]) using to
    the [liftF] function which creates a data of the [Free_monad] from a data of
    the [Functor].

    {[
      let get k = Store_free.liftF (Store.Get (k, id))
      let set k v = Store_free.liftF (Store.Set (k, v, id)))
    ]}

    {3 Using the [Free_monad]}

    Now we are able to define programs and run these programs. For the program
    creation since a [Free_monad] is a Preface [Monad], we can use langage
    extensions like [let*] for a syntetic and expressive program definition. For
    this purpose, the corresponding module should be opened.

    Finally the interpreter can be executed with the [run] functions defined in
    the generated [Free_monad] module.

    {[
      let program =
        let open Store_free.Syntax in
        let* () = set "k1" "v1" in
        get "k1"
      ;;

      ;;
      let l = ref [] in
      Store_free.run (runStore l) program
    ]}

    {2 Conclusion}

    [Preface] makes it possible to construct free monads in several different
    ways. In addition, [liftF] and [run] capabilities are provided for the
    interpretation layer. *)

(** {1 Constructors} *)

(** Incarnation of a [Free_monad] from a [Functor] *)
module Over_functor (F : Preface_specs.FUNCTOR) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t

(** Incarnation of a [Free_monad] from an [Applicative] *)
module Over_applicative (F : Preface_specs.APPLICATIVE) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t

(** Incarnation of a [Free_monad] from a [Monad] *)
module Over_monad (F : Preface_specs.MONAD) :
  Preface_specs.FREE_MONAD with type 'a f = 'a F.t
