(** Exposes [State.t].

    {1 Capabilities}

    - {!val:Functor}
    - {!val:Applicative}
    - {!val:Monad}

    {1 Use cases}

    The State module allows you to deal with the capability to manage a state.
    Management means getting the current state, setting a new state replacing
    the current one and finally modifying the sate thanks to a function.

    {1 Example}

    In this example we propose the manipulation of 2D vectors.

    {2 Vector ADT and basic operations}

    For this purpose such vector can be represented by a pair i.e. generative
    algebra. Then we can define basic operations.

    {[
      module Vector_2D = struct
        type t = int * int

        let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

        let mult k (x, y) = (k * x, k * y)
      end
    ]}

    {2 Vector state creation}

    The corresponding [State] monad is created using the `Via_type` functor.

    {[
      module State = Preface_stdlib.State.Via_type (struct
        type t = Vector_2D.t
      end)
    ]}

    {2 Manipulating state vectors}

    Then we are able to create new state vector instance, setting it, modifying
    it and getting its value. In addition the vector functions can be also
    proposed for state vectors.

    {[
      let add v2 = State.modify (Vector_2D.add v2)

      let mult k = State.modify (Vector_2D.mult k)
    ]}

    Finally we can build simple program using monad syntax. This program takes a
    vector, creates the corresponding state monad and do an addition and a
    multiplication and return the corresponding state.

    {[
      let program =
        let open State in
        let open State.Monad in
        let* () = add (2, 1) in
        let* () = mult 3
            get
      ;;

      let v = fst (program (1,0)) (* v is the vector (9, 3) *)
    ]} *)

(** {1 Implementation} *)

module Over (T : sig
  type t
end) : sig
  (** {2 Types} *)

  type state = T.t
  (** The encapsulated state *)

  type 'a t = state -> 'a * state
  (** The type *)

  module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
  (** {2 Functor API} *)

  module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
  (** {2 Applicative API} *)

  module Monad : Preface_specs.MONAD with type 'a t = 'a t
  (** {2 Monad API} *)

  (** {2 Helpers} *)

  val get : state t
  (** Returns the current state *)

  val set : state -> unit t
  (** Replace the state with the parametric one *)

  val modify : (state -> state) -> unit t
  (** Modify the state applying the parametric function *)
end
