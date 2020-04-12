type t = exn

exception Negative_position of int

let check_position x =
  if x < 0 then Try.error (Negative_position x) else Try.ok x
;;
