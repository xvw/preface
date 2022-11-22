module Side = struct
  type ('a, 'b) t = {
      repr : string
    ; fun_ : 'a -> 'b
  }

  let repr side = side.repr
  let make repr fun_ = { repr; fun_ }
  let pp ppf side = Format.fprintf ppf "%s" (repr side)
end

type ('a, 'b) side = ('a, 'b) Side.t

type ('a, 'b) t = {
    lhs : ('a, 'b) Side.t
  ; rhs : ('a, 'b) Side.t
}

let lhs law = law.lhs
let rhs law = law.rhs
let make lhs rhs = { lhs; rhs }

let pp ppf law =
  Format.fprintf ppf "%a = %a" Side.pp (lhs law) Side.pp (rhs law)
;;

let law ~lhs ~rhs = make lhs rhs
let ( =~ ) name fun_ = Side.make name fun_
