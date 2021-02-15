type 'a t = Random.State.t -> 'a

let rec fold f acc i = if i = 0 then acc else fold f (f acc i) (i - 1)

include (QCheck.Gen : module type of QCheck.Gen with type 'a t := 'a t)

let identity g = map Preface_stdlib.Identity.pure g

let either ?(distribution = 0.5) f_left f_right state =
  let proba = Random.State.float state 1.0 in
  let open Preface_stdlib.Either in
  if proba < distribution then Left (f_left state) else Right (f_right state)
;;

let nonempty_list_size size gen st =
  let init = Preface_stdlib.Nonempty_list.create (gen st) in
  fold
    (fun acc _ -> Preface_stdlib.Nonempty_list.(gen st :: acc))
    init (size st)
;;

let nonempty_list g s = nonempty_list_size nat g s

let small_nonempty_list g s = nonempty_list_size small_nat g s

let continuation f state = Preface_stdlib.Continuation.pure (f state)

let exn state =
  let small_string = string_size ~gen:printable small_nat in
  let proba = Random.State.float state 8.0 in
  if proba >= 0.7
  then Exceptions.A
  else if proba >= 0.6
  then Exceptions.B
  else if proba >= 0.5
  then
    let value = generate1 small_int in
    Exceptions.C value
  else if proba >= 0.4
  then
    let value = generate1 float in
    Exceptions.D value
  else if proba >= 0.3
  then
    let value = generate1 small_string in
    Exceptions.E value
  else if proba >= 0.2
  then
    let value = generate1 (small_list small_int) in
    Exceptions.F value
  else if proba >= 0.1
  then
    let value = generate1 (small_list float) in
    Exceptions.G value
  else
    let value = generate1 (small_list small_string) in
    Exceptions.H value
;;

let result ?(distribution = 0.15) f_ok f_error state =
  let proba = Random.State.float state 1.0 in
  if proba < distribution then Error (f_error state) else Ok (f_ok state)
;;

let validation ?(distribution = 0.15) f_ok f_error state =
  let proba = Random.State.float state 1.0 in
  let open Preface_stdlib.Validation in
  if proba < distribution then Invalid (f_error state) else Valid (f_ok state)
;;

let try_ ?(distribution = 0.15) f_ok state = result ~distribution f_ok exn state

let validate ?(distribution = 0.15) f_valid state =
  validation ~distribution f_valid (small_nonempty_list exn) state
;;

let stream f state =
  let rec aux n = Preface_stdlib.Stream.stream n (lazy (aux (f state))) in
  aux (f state)
;;

let state f state =
  let r = f state in
  (fun s -> (r, s))
;;
