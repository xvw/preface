module Via_map (F : Preface_specs.Functor.CORE) = struct
  type 'a f = 'a F.t

  type 'a t =
    | Return of 'a
    | Bind of 'a t f

  let perform f = Bind (F.map (fun a -> Return a) f)

  let run f =
    let rec loop_run = function
      | Return a -> a
      | Bind g -> f (F.map loop_run g)
    in
    loop_run
  ;;

  let rec map f = function
    | Return v -> Return (f v)
    | Bind f' -> Bind (F.map (map f) f')
  ;;

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

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

  module Selective =
    Selective.Over_applicative_via_select
      (Applicative)
      (struct
        type nonrec 'a t = 'a t

        let select either f =
          let open Monad in
          either
          >>= Either.fold
                ~left:Applicative.((fun a -> ( |> ) a <$> f))
                ~right:Applicative.pure
        ;;
      end)

  include (Monad : Preface_specs.MONAD with type 'a t := 'a t)
end

module Over_functor (A : Preface_specs.FUNCTOR) = Via_map (A)
module Over_applicative (A : Preface_specs.APPLICATIVE) = Via_map (A)
module Over_selective (A : Preface_specs.SELECTIVE) = Via_map (A)
module Over_monad (A : Preface_specs.MONAD) = Via_map (A)
