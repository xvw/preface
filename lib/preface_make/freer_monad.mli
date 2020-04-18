(** Modules for building {!Preface_specs.FREER_MONAD} modules. *)

(** {1 Tutorial}

    [Preface] offers only one way to build a {!Preface_specs.FREER_MONAD}. You
    just have to use the parametrized module {!Via_type}.

    {2 Basics}

    The most common way to build a [Freer_monad] is to use the module
    {!Via_type}.

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
      end
    ]}

    {3 Creating the [Freer_monad]}

    Thanks to the [Preface] library the corresponding [Free_monad] you be simply
    created using the parametric module `Over_functor`.

    {[ module Store_free = Preface_make.Freer_monad.Via_type (Store) ]}

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

    {3 Executing the interpreter}

    The execution should be defined for each interpreter even if the loop seems
    to be the same. The general mecanism is defined by the following function:

    {[
      let run l =
        let rec loop_run = function
          | IO.Return a -> a
          | IO.Bind (intermediate, continuation) ->
            loop_run (continuation (runStore l intermediate))
        in
        loop_run
      ;;
    ]}

    {3 Using the [Freer_monad] and the corresponding execution}

    Now we are able to define programs and run these programs. For the program
    creation since a [Free_monad] is a Preface [Monad], we can use langage
    extensions like [let*] for a syntetic and expressive program definition. For
    this purpose, the corresponding module should be opened.

    Finally the interpreter can be defines in order to execute given program
    with a specific interpreter.

    {[
      let program =
        let open Store_free.Syntax in
        let* () = set "k1" "v1" in
        get "k1"
      ;;

      ;;
      let l = ref [] in
      run l program
    ]}

    {2 Conclusion}

    [Preface] makes it possible to construct freer monads but the execution
    should be provided. *)

(** {1 Constructors} *)

module Via_type (T : Preface_specs.Freer_monad.TYPE) :
  Preface_specs.FREER_MONAD with type 'a f = 'a T.t
