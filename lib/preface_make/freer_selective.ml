module Either = Preface_core.Shims.Either
open Preface_core.Fun.Infix

module Over (Type : Preface_specs.Types.T1) = struct
  type 'a f = 'a Type.t

  type _ t =
    | Pure : 'a -> 'a t
    | Select : (('b -> 'a, 'a) Either.t t * 'b f) -> 'a t

  let pre_apply f = Either.map ~left:(fun h x -> f (h x)) ~right:f

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let rec map : type a b. (a -> b) -> a t -> b t =
     fun f -> function
      | Pure x -> Pure (f x)
      | Select (x, y) ->
        let g = pre_apply f in
        Select (map g x, y)
   ;;
  end)

  module Core = struct
    type nonrec 'a t = 'a t

    let pure x = Pure x

    let select a b =
      let g f x =
        let open Either in
        match f x with
        | Right x -> Right (Right x)
        | Left f -> Left (pre_apply f)
      in
      let rec aux : type a b c. (a -> (b -> c, c) Either.t) -> a t -> b t -> c t
          =
       fun f x -> function
        | Select (y, z) -> Select (aux (g f) x y, z)
        | Pure y ->
          let h = Either.case (( |> ) y) Fun.id in
          Functor.(h % f <$> x)
      in

      aux (Either.map_left ( |> )) a b
    ;;
  end

  module S = Selective.Over_functor_via_select (Functor) (Core)

  module To_selective (Selective : Preface_specs.SELECTIVE) = struct
    type natural_transformation = { transform : 'a. 'a f -> 'a Selective.t }

    let rec run : type a. natural_transformation -> a t -> a Selective.t =
     fun transformation -> function
      | Pure x -> Selective.pure x
      | Select (either, fs) ->
        Selective.select
          (run transformation either)
          Selective.(( |> ) <$> transformation.transform fs)
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

  let promote x = Select (Pure (Either.left Fun.id), x)
end

module Over_selective (F : Preface_specs.SELECTIVE) = struct
  include Over (F)
  module R = To_selective (F)

  let run x =
    let t = R.{ transform = Fun.id } in
    R.run t x
  ;;
end
