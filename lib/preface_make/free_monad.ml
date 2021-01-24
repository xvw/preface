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

  include (Monad : Preface_specs.MONAD with type 'a t := 'a t)
end

module Over_functor (A : Preface_specs.FUNCTOR) = Via_map (A)
module Over_applicative (A : Preface_specs.APPLICATIVE) = Via_map (A)
module Over_monad (A : Preface_specs.MONAD) = Via_map (A)

module Traversable (Free : Preface_specs.FREE_MONAD) = struct
  module Applicative
      (A : Preface_specs.APPLICATIVE)
      (T : functor
        (Ap : Preface_specs.APPLICATIVE with type 'a t = 'a A.t)
        ->
        Preface_specs.TRAVERSABLE
          with type 'a t = 'a A.t
           and type 'a iter = 'a Free.f) =
    Traversable.Over_applicative
      (A)
      (struct
        module Tr = T (A)

        type 'a t = 'a A.t

        type 'a iter = 'a Free.t

        let rec traverse f = function
          | Free.Return a -> A.map (fun x -> Free.Return x) (f a)
          | Free.Bind fa ->
            A.map (fun x -> Free.Bind x) (Tr.traverse (traverse f) fa)
        ;;
      end)

  module Monad
      (M : Preface_specs.MONAD)
      (T : functor
        (Md : Preface_specs.MONAD with type 'a t = 'a M.t)
        ->
        Preface_specs.TRAVERSABLE
          with type 'a t = 'a M.t
           and type 'a iter = 'a Free.f) =
    Traversable.Over_monad
      (M)
      (struct
        module Tr = T (M)

        type 'a t = 'a M.t

        type 'a iter = 'a Free.t

        let rec traverse f = function
          | Free.Return a -> M.map (fun x -> Free.Return x) (f a)
          | Free.Bind fa ->
            M.map (fun x -> Free.Bind x) (Tr.traverse (traverse f) fa)
        ;;
      end)
end
