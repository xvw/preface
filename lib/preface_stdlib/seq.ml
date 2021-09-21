module S = Stdlib.Seq

type 'a t = 'a S.t

let pure x = S.return x

let rev seq =
  let rec aux acc seq =
    (match seq () with S.Nil -> acc | S.Cons (x, xs) -> aux (S.cons x acc) xs)
  in
  aux S.empty seq
;;

module Functor = Preface_make.Functor.Via_map (Stdlib.Seq)

module Alternative = Preface_make.Alternative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure x = pure x

  let apply fs xs =
    let rec aux a b () =
      (match a () with S.Nil -> S.Nil | S.Cons (x, xs) -> merge xs b x b ())
    and merge a bs f b () =
      match b () with
      | S.Nil -> aux a bs ()
      | S.Cons (x, xs) -> S.Cons (f x, merge a bs f xs)
    in
    aux fs xs
  ;;

  let neutral = S.empty

  let combine l r = S.append l r
end)

module Applicative_traversable (A : Preface_specs.APPLICATIVE) =
  Preface_make.Traversable.Over_applicative
    (A)
    (struct
      type 'a t = 'a A.t

      type 'a iter = 'a S.t

      let traverse f seq =
        let open A.Infix in
        let rec traverse acc seq =
          match seq () with
          | S.Nil -> rev <$> acc
          | S.Cons (x, xs) -> traverse (S.cons <$> f x <*> acc) xs
        in

        traverse (A.pure S.empty) seq
      ;;
    end)

module Applicative =
  Preface_make.Traversable.Join_with_applicative
    (Alternative)
    (Applicative_traversable)

module Foldable = Preface_make.Foldable.Via_fold_map (struct
  type nonrec 'a t = 'a t

  let fold_map' neutral combine f seq =
    let rec aux acc seq () =
      match seq () with
      | S.Nil -> acc
      | S.Cons (x, xs) ->
        let result = combine acc (f x) in
        aux result xs ()
    in
    aux neutral seq ()
  ;;
end)

module Monad_plus = Preface_make.Monad_plus.Via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  let bind = S.flat_map

  let neutral = S.empty

  let combine l r = S.append l r
end)

module Monad_traversable (M : Preface_specs.MONAD) =
  Preface_make.Traversable.Over_monad
    (M)
    (struct
      type 'a t = 'a M.t

      type 'a iter = 'a S.t

      let traverse f seq =
        let open M.Infix in
        let rec traverse acc seq =
          match seq () with
          | S.Nil -> acc >|= rev
          | S.Cons (x, xs) -> traverse (f x >>= (fun a -> acc >|= S.cons a)) xs
        in
        traverse (M.return S.empty) seq
      ;;
    end)

module Monad =
  Preface_make.Traversable.Join_with_monad (Monad_plus) (Monad_traversable)
module Selective =
  Preface_make.Selective.Over_applicative_via_select
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))

module Monoid (T : Preface_specs.Types.T0) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type nonrec t = T.t t

  let combine l r = S.append l r

  let neutral = S.empty
end)

let equal eq =
  let rec aux left right =
    match (left (), right ()) with
    | (S.Nil, S.Nil) -> true
    | (S.Cons (a, axs), S.Cons (b, bxs)) ->
      if eq a b then aux axs bxs else false
    | _ -> false
  in
  aux
;;

let pp f ppf seq =
  let rec aux ppf seq =
    match seq () with
    | S.Nil -> ()
    | S.Cons (x, xs) -> Format.fprintf ppf ";@ %a%a" f x aux xs
  in
  match seq () with
  | S.Nil -> ()
  | S.Cons (x, xs) -> Format.fprintf ppf "Seq@[[%a%a]@]" f x aux xs
;;
