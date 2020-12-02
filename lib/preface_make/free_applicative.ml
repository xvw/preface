open Preface_core.Fun

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_APPLICATIVE with type 'a f = 'a F.t = struct
  type 'a f = 'a F.t

  type _ t =
    | Pure : 'a -> 'a t
    | Apply : ('a -> 'b) t * 'a f -> 'b t

  let promote x = Apply (Pure id, x)

  module Core = struct
    type nonrec 'a t = 'a t

    let pure x = Pure x

    let rec map : type a b. (a -> b) -> a t -> b t =
     fun f -> function
      | Pure x -> Pure (f x)
      | Apply (fa, x) ->
        let fs = map (fun g x -> f (g x)) fa in
        Apply (fs, x)
   ;;

    let rec apply : type a b. (a -> b) t -> a t -> b t =
     fun fs xs ->
      match fs with
      | Pure f -> map f xs
      | Apply (fa, x) ->
        let gs =
          let left = map flip fa in
          apply left xs
        in
        Apply (gs, x)
   ;;

    let product a b = apply (apply (Pure (fun x y -> (x, y))) a) b
  end

  module Operation = Applicative.Operation (Core)
  module Lift = Applicative.Lift (Core)
  module A =
    Applicative.Via (Core) (Operation) (Lift)
      (Applicative.Infix (Core) (Operation) (Lift))
      (Applicative.Syntax (Core))

  module Transformation (Applicative : Preface_specs.Applicative.CORE) = struct
    type natural_transformation = { transform : 'a. 'a f -> 'a Applicative.t }

    let rec run : type a. natural_transformation -> a t -> a Applicative.t =
     fun transformation -> function
      | Pure x -> Applicative.pure x
      | Apply (fs, x) ->
        Applicative.apply (run transformation fs) (transformation.transform x)
   ;;
  end

  include (A : Preface_specs.APPLICATIVE with type 'a t := 'a t)
end

module Over_applicative (A : Preface_specs.Applicative.CORE) = struct
  include Over_functor (A)
  module R = Transformation (A)

  let run x =
    let t = R.{ transform = id } in
    R.run t x
  ;;
end
