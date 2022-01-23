type 'a t = 'a list

let pure x = [ x ]

module Foldable = Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = 'a t

  let fold_right f x acc = Stdlib.List.fold_right f x acc
end)

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = Stdlib.List.map
end)

module Alternative = Preface_make.Alternative.Via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply fs xs =
    Stdlib.List.(concat @@ map (fun f -> map (fun x -> f x) xs) fs)
  ;;

  let neutral = []
  let combine l r = l @ r
end)

module Applicative_traversable (A : Preface_specs.APPLICATIVE) =
  Preface_make.Traversable.Over_applicative
    (A)
    (struct
      type 'a t = 'a A.t
      type 'a iter = 'a list

      let traverse f =
        let open A.Infix in
        let rec traverse acc = function
          | [] -> Stdlib.List.rev <$> acc
          | x :: xs -> traverse (A.lift2 Stdlib.List.cons (f x) acc) xs
        in
        traverse (A.pure [])
      ;;
    end)

module Applicative =
  Preface_make.Traversable.Join_with_applicative
    (Alternative)
    (Applicative_traversable)

module Monad_plus = Preface_make.Monad_plus.Via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  (* Implementation from OCaml 4.10.0 *)
  let bind f =
    let rec aux_bind acc = function
      | [] -> Stdlib.List.rev acc
      | x :: tail ->
        let xs = f x in
        aux_bind (Stdlib.List.rev_append xs acc) tail
    in
    aux_bind []
  ;;

  let neutral = []
  let combine l r = l @ r
end)

module Monad_traversable (M : Preface_specs.MONAD) =
  Preface_make.Traversable.Over_monad
    (M)
    (struct
      type 'a t = 'a M.t
      type 'a iter = 'a list

      let traverse f =
        let open M.Infix in
        let rec traverse acc = function
          | [] -> acc >|= Stdlib.List.rev
          | x :: xs -> traverse (M.lift2 Stdlib.List.cons (f x) acc) xs
        in
        traverse (M.return [])
      ;;
    end)

module Monad =
  Preface_make.Traversable.Join_with_monad (Monad_plus) (Monad_traversable)

module Selective =
  Preface_make.Selective.Over_applicative_via_select
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))

module Invariant = Preface_make.Invariant.From_functor (Functor)

module Monoid (T : Preface_specs.Types.T0) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type nonrec t = T.t t

  let combine l r = l @ r
  let neutral = []
end)

let equal f a b =
  let rec eq = function
    | [], [] -> true
    | x :: xs, y :: ys -> f x y && eq (xs, ys)
    | _ -> false
  in
  eq (a, b)
;;

let pp pp' formater list =
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.(fprintf formater "@[[%a]@]" (pp_print_list ~pp_sep pp') list)
;;
