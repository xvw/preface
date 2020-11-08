type 'a t = 'a Preface_core.Nonempty_list.t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

include (
  Preface_core.Nonempty_list :
    module type of Preface_core.Nonempty_list with type 'a t := 'a t )

let pure x = create x

module Alt = Preface_make.Alt.Via_combine (struct
  type nonrec 'a t = 'a t

  let combine = append
end)

module Foldable = Preface_make.Foldable.Via_fold_right (struct
  type nonrec 'a t = 'a t

  let fold_right f x acc = fold_right f x acc
end)

module Functor = Preface_make.Functor.Via_map (struct
  type nonrec 'a t = 'a t

  let map = map
end)

module Applicative = struct
  module A = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply fs xs = flatten @@ map (fun f -> map (fun x -> f x) xs) fs
  end)

  module Traversable (A : Preface_specs.APPLICATIVE) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a A.t and type 'a iter = 'a t =
  struct
    module T = struct
      type 'a t = 'a A.t

      type 'a iter = 'a Preface_core.Nonempty_list.t

      let traverse =
        let open A.Infix in
        let rec traverse_aux f = function
          | Last x -> (fun x -> Last x) <$> f x
          | x :: xs -> A.lift2 cons (f x) (traverse_aux f xs)
        in
        traverse_aux
      ;;
    end

    include Preface_make.Traversable.Over_applicative (A) (T)
  end

  include A
end

module Monad = struct
  module M = Preface_make.Monad.Via_map_and_join (struct
    type nonrec 'a t = 'a t

    let return = pure

    let map = map

    let join = flatten
  end)

  module Traversable (M : Preface_specs.MONAD) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a M.t and type 'a iter = 'a t =
  struct
    module T = struct
      type 'a t = 'a M.t

      type 'a iter = 'a Preface_core.Nonempty_list.t

      let traverse =
        let open M.Syntax in
        let rec traverse_aux f = function
          | Last x ->
            let+ elt = f x in
            Last elt
          | x :: xs ->
            let* h = f x in
            let* t = traverse_aux f xs in
            M.return (cons h t)
        in
        traverse_aux
      ;;
    end

    include Preface_make.Traversable.Over_monad (M) (T)
  end

  include M
end

module Selective =
  Preface_make.Selective.Over_applicative
    (Applicative)
    (Preface_make.Selective.Select_from_monad (Monad))

module Semigroup (T : Preface_specs.Types.T0) :
  Preface_specs.SEMIGROUP with type t = T.t t =
Preface_make.Semigroup.Via_combine (struct
  type nonrec t = T.t t

  let combine = append
end)
