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

module Applicative = struct
  module A = Preface_make.Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply fs xs =
      Stdlib.List.(concat @@ map (fun f -> map (fun x -> f x) xs) fs)
    ;;
  end)

  module Traversable (A : Preface_specs.APPLICATIVE) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a A.t and type 'a iter = 'a t =
  struct
    module T = struct
      type 'a t = 'a A.t

      type 'a iter = 'a list

      let traverse =
        let open A.Infix in
        let rec traverse f = function
          | [] -> A.pure []
          | x :: xs -> Stdlib.List.cons <$> f x <*> traverse f xs
        in
        traverse
      ;;
    end

    include Preface_make.Traversable.Over_applicative (A) (T)
  end

  include A
end

module Monad = struct
  module M = Preface_make.Monad.Via_bind (struct
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
  end)

  module Traversable (M : Preface_specs.MONAD) :
    Preface_specs.TRAVERSABLE with type 'a t = 'a M.t and type 'a iter = 'a t =
  struct
    module T = struct
      type 'a t = 'a M.t

      type 'a iter = 'a list

      let traverse =
        let open M.Syntax in
        let rec traverse f = function
          | [] -> M.return []
          | x :: xs ->
            let* h = f x in
            let* t = traverse f xs in
            M.return (Stdlib.List.cons h t)
        in
        traverse
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

module Monoid (T : Preface_specs.Types.T0) =
Preface_make.Monoid.Via_combine_and_neutral (struct
  type nonrec t = T.t t

  let combine l r = l @ r

  let neutral = []
end)

let eq f a b =
  let rec eq = function
    | ([], []) -> true
    | (x :: xs, y :: ys) -> f x y && eq (xs, ys)
    | _ -> false
  in
  eq (a, b)
;;

let pp pp' formater list =
  let pp_sep ppf () = Format.fprintf ppf "@; " in
  Format.(fprintf formater "@[[%a]@]" (pp_print_list ~pp_sep pp') list)
;;
