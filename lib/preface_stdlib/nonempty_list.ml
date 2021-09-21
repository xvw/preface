type 'a t = 'a Preface_core.Nonempty_list.t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

include (
  Preface_core.Nonempty_list :
    module type of Preface_core.Nonempty_list with type 'a t := 'a t )

let pure x = create x

module Foldable = Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = 'a t

  let fold_right f x acc = fold_right f x acc
end)

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = map
end)

module Alt =
  Preface_make.Alt.Over_functor
    (Functor)
    (struct
      type nonrec 'a t = 'a t

      let combine = append
    end)

module Applicative_internal = Preface_make.Applicative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fs xs = flatten @@ map (fun f -> map (fun x -> f x) xs) fs
end)

module Applicative_traversable (A : Preface_specs.APPLICATIVE) =
  Preface_make.Traversable.Over_applicative
    (A)
    (struct
      type 'a t = 'a A.t

      type 'a iter = 'a Preface_core.Nonempty_list.t

      let traverse f l =
        let open A.Infix in
        let rec traverse_aux acc = function
          | Last x -> rev <$> A.lift2 cons (f x) acc
          | x :: xs -> traverse_aux (A.lift2 cons (f x) acc) xs
        in
        match l with
        | Last x -> create <$> f x
        | x :: xs -> traverse_aux (create <$> f x) xs
      ;;
    end)

module Applicative =
  Preface_make.Traversable.Join_with_applicative
    (Applicative_internal)
    (Applicative_traversable)

module Monad_internal = Preface_make.Monad.Via_map_and_join (struct
  type nonrec 'a t = 'a t

  let return = pure

  let map = map

  let join = flatten
end)

module Monad_traversable (M : Preface_specs.MONAD) =
  Preface_make.Traversable.Over_monad
    (M)
    (struct
      type 'a t = 'a M.t

      type 'a iter = 'a Preface_core.Nonempty_list.t

      let traverse f l =
        let open M.Infix in
        let rec traverse_aux acc = function
          | Last x -> rev <$> M.lift2 cons (f x) acc
          | x :: xs -> traverse_aux (M.lift2 cons (f x) acc) xs
        in
        match l with
        | Last x -> f x >|= create
        | x :: xs -> traverse_aux (f x >|= create) xs
      ;;
    end)

module Monad =
  Preface_make.Traversable.Join_with_monad (Monad_internal) (Monad_traversable)
module Selective =
  Preface_make.Selective.Over_applicative_via_select
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))

module Comonad = Preface_make.Comonad.Via_extend (struct
  type nonrec 'a t = 'a t

  let extract = function Last x | x :: _ -> x

  let rec extend f nel =
    (match nel with Last _ -> Last (f nel) | _ :: xs -> f nel :: extend f xs)
  ;;
end)

module Semigroup (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t t =
Preface_make.Semigroup.Via_combine (struct
  type nonrec t = T.t t

  let combine = append
end)
