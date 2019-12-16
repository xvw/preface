(** Exposes [Option.t].

    The Option module allows you to deal with the presence or absence
    of value. Here is some use-cases:

    {2 Parallel validation}
    Using [Applicative] you can write parallel validation pipeline. For example,
    let's imagine the module:

    {[
      type human = {
        age: int
      ; name: string
      }
      let create age name = { age; name }
    ]}

    Let's add some rules:
    - an [age] must be positive;
    - a name must have at least 2 characters.

    {[
      let age_validation age = 
        if age >= 0 then Some age 
        else None

      let name_validation name = 
        if String.length name >= 2 then Some name 
        else None
    ]}

    Now, we have a parallel way to validate inputs of [create]:
    
    {[
      let try_create age name = 
        let open Option.Applicative.Infix in 
        create 
        <$> age_validation age 
        <*> name_validation name
    ]}

    {2 Sequential validation}
    using [Monad] we can write sequential validation, for example:

    {[
      let divide_by x y = 
        if x = 0 then None 
        else Some (y / x)

      let result = 
        let open Option.Monad in
        return 45
        >|= (fun x -> x + 22) 
        >>= divide_by 0
        >|= succ 
    ]}
*)

(** {1 Type} *)

type 'a t = 'a option

(** {1 Implementation} *)

module Functor : Preface_specs.FUNCTOR with type 'a t = 'a t
(** {2 Functor API} *)

module Applicative : Preface_specs.APPLICATIVE with type 'a t = 'a t
(** {2 Applicative API} *)

module Monad : Preface_specs.MONAD with type 'a t = 'a t
(** {2 Monad API} *)

(** {1 Helpers} *)

val eq : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality.*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Pretty printing. *)
