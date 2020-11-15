open Preface_core.Fun

module Over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (F : Preface_specs.Functor.CORE) =
struct
  type 'a f = 'a F.t

  type ('a, 'b) either = ('a, 'b) Either.t

  type _ t =
    | Pure : 'a -> 'a t
    | Select : ('a -> 'b) f * ('a, 'b) either t -> 'b t

  module Functor = struct
    type nonrec 'a t = 'a t

    let rec map : type a b. (a -> b) -> a t -> b t =
     fun f -> function
      | Pure x -> Pure (f x)
      | Select (fs, either) ->
        Select (F.map (fun g x -> f (g x)) fs, map (Either.map_right f) either)
   ;;
  end

  module Core = struct
    type nonrec 'a t = 'a t

    type nonrec ('a, 'b) either = ('a, 'b) either

    let bimap f g = Either.case (Either.left % f) (Either.right % g)

    let pure x = Pure x

    let rec select : type a b. (a, b) either t -> (a -> b) t -> b t =
     fun x -> function
      | Pure y -> Functor.map (Either.case y id) x
      | Select (fs, either) ->
        let f u = Either.(map_right right) u
        and g x a = bimap (fun b -> (b, a)) (fun k -> k a) x
        and h f (x, y) = f x y in
        Select (F.map h fs, select (Functor.map f x) (Functor.map g either))
   ;;
  end

  module S = Selective.Over_functor_and_either (Either) (Functor) (Core)

  module Transformation
      (Selective : Preface_specs.Selective.CORE
                     with type ('a, 'b) either = ('a, 'b) either) =
  struct
    type natural_transformation = { transform : 'a. 'a f -> 'a Selective.t }

    let rec run : type a. natural_transformation -> a t -> a Selective.t =
     fun transformation -> function
      | Pure x -> Selective.pure x
      | Select (fs, either) ->
        Selective.select
          (run transformation either)
          (transformation.transform fs)
   ;;
  end

  include (
    S :
      Preface_specs.SELECTIVE
        with type 'a t := 'a t
         and type ('a, 'b) either := ('a, 'b) Either.t )

  let promote x = Select (F.map const x, Pure (Either.left ()))
end

module Over_applicative_and_either = Over_functor_and_either
module Over_functor = Over_functor_and_either (Preface_core.Either)
module Over_applicative = Over_functor

module Over_selective_and_either
    (Either : Preface_core.Requirements.EITHER)
    (F : Preface_specs.Selective.CORE
           with type ('a, 'b) either = ('a, 'b) Either.t) =
struct
  include Over_functor_and_either (Either) (F)
  module R = Transformation (F)

  let run x =
    let t = R.{ transform = id } in
    R.run t x
  ;;
end

module Over_selective = Over_selective_and_either (Preface_core.Either)
