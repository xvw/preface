type 'a t =
  | Last of 'a
  | ( :: ) of ('a * 'a t)

let create x = Last x

let rec rev_append l1 l2 =
  (match l1 with Last x -> x :: l2 | x :: xs -> rev_append xs (x :: l2))
;;

let hd = function Last x | x :: _ -> x

let tl = function Last _ -> None | _ :: xs -> Some xs

let rev = function
  | Last x -> Last x
  | x :: xs ->
    let rec aux_rev acc = function
      | Last x -> x :: acc
      | x :: xs -> aux_rev (x :: acc) xs
    in
    aux_rev (Last x) xs
;;

let from_list = function
  | [] -> None
  | List.( :: ) (x, xs) ->
    let rec aux_from acc = function
      | [] -> Some (rev acc)
      | List.( :: ) (x, xs) -> aux_from (x :: acc) xs
    in
    aux_from (Last x) xs
;;

let to_list list =
  let rec to_aux acc = function
    | Last x -> List.(rev (x :: acc))
    | x :: xs -> to_aux List.(x :: acc) xs
  in
  to_aux [] list
;;

let length list =
  let rec length_aux acc = function
    | Last _ -> acc + 1
    | _ :: xs -> length_aux (acc + 1) xs
  in
  length_aux 0 list
;;

let cons x xs = x :: xs

let iteri f list =
  let rec iter_aux acc = function
    | Last x -> f acc x
    | x :: xs ->
      let () = f acc x in
      iter_aux (acc + 1) xs
  in
  iter_aux 0 list
;;

let iter f list =
  let f' _ x = f x in
  iteri f' list
;;

let mapi f = function
  | Last x -> Last (f 0 x)
  | x :: xs ->
    let rec map_aux i acc = function
      | Last x -> rev (f i x :: acc)
      | x :: xs -> map_aux (i + 1) (f i x :: acc) xs
    in
    map_aux 1 (Last (f 0 x)) xs
;;

let map f list =
  let f' _ x = f x in
  mapi f' list
;;

let fold_left f acc = function
  | Last x -> f acc x
  | x :: xs ->
    let rec fold_aux acc = function
      | Last x -> f acc x
      | x :: xs -> fold_aux (f acc x) xs
    in
    fold_aux (f acc x) xs
;;

let reduce f = function Last x -> x | x :: xs -> fold_left f x xs

let fold_right f list acc =
  match list with
  | Last x -> f x acc
  | x :: xs ->
    let rec fold_aux acc = function
      | Last x -> f x acc
      | x :: xs -> f x (fold_aux acc xs)
    in
    f x (fold_aux acc xs)
;;

let append a' b =
  let a = rev a' in
  let rec append_aux acc = function
    | Last x -> x :: acc
    | x :: xs -> append_aux (x :: acc) xs
  in
  append_aux b a
;;

let flatten = function
  | Last x -> x
  | xs ->
    let rec flatten_aux = function
      | Last x -> x
      | x :: xs -> append x (flatten_aux xs)
    in
    flatten_aux xs
;;

let equal f a b =
  let rec eq_aux = function
    | (Last x, Last y) -> f x y
    | (x :: xs, y :: ys) -> f x y && eq_aux (xs, ys)
    | _ -> false
  in
  eq_aux (a, b)
;;

let pp pp' formater list =
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.(
    fprintf formater "@[[%a]@]" (pp_print_list ~pp_sep pp') (to_list list))
;;
