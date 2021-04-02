open Preface_core.Fun
module Either = Preface_core.Shims.Either

module Over_functor (F : Preface_specs.Functor.CORE) = struct
  type 'a f = 'a F.t

  type _ t =
    | Pure : 'a -> 'a t
    | Select : ('a, 'b) Either.t t * ('a -> 'b) f -> 'b t

  module Functor = struct
    type nonrec 'a t = 'a t

    let rec map : type a b. (a -> b) -> a t -> b t =
     fun f -> function
      | Pure x -> Pure (f x)
      | Select (either, fs) ->
        Select (map (Either.map_right f) either, F.map (fun g x -> f (g x)) fs)
   ;;
  end

  module Core = struct
    type nonrec 'a t = 'a t

    let bimap f g = Either.case (Either.left % f) (Either.right % g)

    let pure x = Pure x

    let rec select : type a b. (a, b) Either.t t -> (a -> b) t -> b t =
     fun x -> function
      | Pure y -> Functor.map (Either.case y id) x
      | Select (either, fs) ->
        let f u = Either.(map_right right) u
        and g x a = bimap (fun b -> (b, a)) (fun k -> k a) x
        and h f (x, y) = f x y in
        Select (select (Functor.map f x) (Functor.map g either), F.map h fs)
   ;;
  end

  module S = Selective.Over_functor_via_select (Functor) (Core)

  module To_selective (Selective : Preface_specs.Selective.CORE) = struct
    type natural_transformation = { transform : 'a. 'a f -> 'a Selective.t }

    let rec run : type a. natural_transformation -> a t -> a Selective.t =
     fun transformation -> function
      | Pure x -> Selective.pure x
      | Select (either, fs) ->
        Selective.select
          (run transformation either)
          (transformation.transform fs)
   ;;
  end

  module To_monoid (Monoid : Preface_specs.Monoid.CORE) = struct
    type natural_transformation = { transform : 'a. 'a f -> Monoid.t }

    module C = Selective.Const (Monoid)
    module T = To_selective (C)

    let run nt x =
      let n =
        let transform x = C.Const (nt.transform x) in
        let open T in
        { transform }
      in

      C.get (T.run n x)
    ;;
  end

  include (S : Preface_specs.SELECTIVE with type 'a t := 'a t)

  let promote x = Select (Pure (Either.left ()), F.map const x)
end

module Over_applicative = Over_functor

module Over_selective (F : Preface_specs.Selective.CORE) = struct
  include Over_functor (F)
  module R = To_selective (F)

  let run x =
    let t = R.{ transform = id } in
    R.run t x
  ;;
end
