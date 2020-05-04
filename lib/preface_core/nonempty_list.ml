type 'a t = 'a * 'a list

let create x = (x, [])

let from_list = function [] -> None | x :: xs -> Some (x, xs)

let to_list (x, xs) = x :: xs

let hd (x, _) = x

let tl (_, xs) = xs

let length (_, xs) = 1 + List.length xs

let cons a (x, xs) = (a, x :: xs)

let rev (x, xs) =
  (match List.rev xs with [] -> (x, []) | y :: ys -> (y, ys @ [ x ]))
;;

let flatten ((x, xs), ys) =
  let l = List.(map to_list ys |> flatten) in
  (x, xs @ l)
;;

let iteri f (x, xs) =
  let () = f 0 x in
  let f' i x = f (succ i) x in
  List.iteri f' xs
;;

let iter f x =
  let f' _ x = f x in
  iteri f' x
;;

let mapi f (x, xs) =
  let f' i x = f (succ i) x in
  (f 0 x, List.mapi f' xs)
;;

let map f x =
  let f' _ x = f x in
  mapi f' x
;;

let fold_left f acc (x, xs) =
  let first = f acc x in
  List.fold_left f first xs
;;

let reduce f (x, xs) = List.fold_left f x xs

let fold_right f (x, xs) acc =
  let first = List.fold_right f xs acc in
  f x first
;;

let eq f (x, xs) (y, ys) =
  let rec aux_eq = function
    | ([], []) -> true
    | (x :: xs, y :: ys) -> f x y && aux_eq (xs, ys)
    | _ -> false
  in
  f x y && aux_eq (xs, ys)
;;

let pp pp' formater (x, xs) =
  let pp_sep ppf () = Format.fprintf ppf "@; " in
  Format.(fprintf formater "@[%a :: [%a]@]" pp' x (pp_print_list ~pp_sep pp') xs)
;;
