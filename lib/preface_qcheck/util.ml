let test ~count ?print generator law f =
  let l = law () in
  let name = Preface_laws.Law.name l in
  let lhs, rhs = Preface_laws.Law.get_sides l in
  QCheck2.Test.make ~count ~name ?print generator (fun generated ->
      f lhs rhs generated )
;;

let gen_either left right =
  let open QCheck2.Gen in
  frequency [ (5, left >|= Either.left); (5, right >|= Either.right) ]
;;

let gen_exn =
  let open QCheck2.Gen in
  frequency
    [ (5, pure Not_found); (5, string_printable >|= fun x -> Failure x) ]
;;

let gen_result ok error =
  let open QCheck2.Gen in
  frequency [ (7, ok >|= Result.ok); (3, error >|= Result.error) ]
;;

let gen_try ok = gen_result ok gen_exn

let pp_either left right ppf = function
  | Stdlib.Either.Left x -> Format.fprintf ppf "Left %a" left x
  | Stdlib.Either.Right x -> Format.fprintf ppf "Right %a" right x
;;

let pp_exn ppf x = Format.fprintf ppf "%s" (Printexc.to_string_default x)

let pp_result ok error ppf = function
  | Ok x -> Format.fprintf ppf "Ok %a" ok x
  | Error x -> Format.fprintf ppf "Error %a" error x
;;

let pp_try ok ppf = function
  | Ok x -> Format.fprintf ppf "Ok %a" ok x
  | Error x -> Format.fprintf ppf "Error %s" (Printexc.to_string_default x)
;;

let seed_hash x obs y = Hashtbl.seeded_hash x (QCheck2.Observable.hash obs y)
let print obs x = QCheck2.Observable.print obs x
let eq obs = QCheck2.Observable.equal obs

let obs_either left right =
  let hash = function
    | Stdlib.Either.Left x -> seed_hash 42 left x
    | Stdlib.Either.Right x -> seed_hash 43 right x
  and print = function
    | Stdlib.Either.Left x -> Format.asprintf "Left %s" (print left x)
    | Stdlib.Either.Right x -> Format.asprintf "Right %s" (print right x)
  and eq = Stdlib.Either.equal ~left:(eq left) ~right:(eq right) in
  QCheck2.Observable.make ~eq ~hash print
;;

let obs_result ok error =
  let hash = function
    | Ok x -> seed_hash 42 ok x
    | Error x -> seed_hash 43 error x
  and print = function
    | Ok x -> Format.asprintf "Ok %s" (print ok x)
    | Error x -> Format.asprintf "Error %s" (print error x)
  and eq = Result.equal ~ok:(eq ok) ~error:(eq error) in
  QCheck2.Observable.make ~eq ~hash print
;;

let equal_exn a b = Int.equal (Printexc.exn_slot_id a) (Printexc.exn_slot_id b)

let obs_exn =
  let hash exn = Hashtbl.seeded_hash 100 exn
  and print = Printexc.to_string
  and eq = equal_exn in
  QCheck2.Observable.make ~eq ~hash print
;;

let obs_try ok = obs_result ok obs_exn
let equal_pair f g (a, b) (x, y) = f a x && g b y
let equal_either left right = Either.equal ~left ~right
