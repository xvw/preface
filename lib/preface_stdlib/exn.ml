type t = exn

exception Negative_position of int

let check_position x = if x < 0 then Error (Negative_position x) else Ok x
let equal = ( = )
let pp ppf exn = Format.fprintf ppf "%s" (Printexc.to_string exn)
