module Via_map (F : Preface_specs.Functor.CORE) = struct
  type 'a f = 'a F.t

  type 'a t =
    | Return of 'a
    | Bind of 'a t f

  let liftF f = Bind (F.map (fun a -> Return a) f)

  let run f =
    let rec loop_run = function
      | Return a -> a
      | Bind g -> f (F.map loop_run g)
    in
    loop_run
  ;;

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let rec map f = function
      | Return v -> Return (f v)
      | Bind f' -> Bind (F.map (map f) f')
    ;;
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let rec map f = function
      | Return v -> Return (f v)
      | Bind f' -> Bind (F.map (map f) f')
    ;;

    let pure a = Return a

    let rec apply f a =
      match f with
      | Return f' -> map f' a
      | Bind f' -> Bind (F.map (fun f -> apply f a) f')
    ;;
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return a = Return a

    let rec bind f = function
      | Return a -> f a
      | Bind a -> Bind (F.map (bind f) a)
    ;;
  end)

  include (Monad : Preface_specs.MONAD with type 'a t := 'a t)
end

module Over_functor (A : Preface_specs.FUNCTOR) = Via_map (A)
module Over_applicative (A : Preface_specs.APPLICATIVE) = Via_map (A)
module Over_monad (A : Preface_specs.MONAD) = Via_map (A)
