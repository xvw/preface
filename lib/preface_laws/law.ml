module Side = struct
  type ('a, 'b) t = {
      repr : string
    ; fun_ : 'a -> 'b
  }

  let repr side = side.repr

  let fun_ side = side.fun_

  let make repr fun_ = { repr; fun_ }

  let pp ppf side = Format.fprintf ppf "%s" (repr side)
end

type ('a, 'b) t = {
    name : string
  ; lhs : ('a, 'b) Side.t
  ; rhs : ('a, 'b) Side.t
}

let name law = law.name

let lhs law = law.lhs

let rhs law = law.rhs

let make name lhs rhs = { name; lhs; rhs }

let pp ppf law =
  Format.fprintf ppf "%s: %a = %a" (name law) Side.pp (lhs law) Side.pp
    (rhs law)
;;
