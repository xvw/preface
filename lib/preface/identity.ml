type 'a t = 'a

let pure x = x

module Functor = Functor.Make_via_map (struct
  type nonrec 'a t = 'a t

  let map f = f
end)

module Applicative = Applicative.Make_via_apply (struct
  type nonrec 'a t = 'a t

  let pure = pure

  let apply f = f
end)

module Selective (E : Specs.EITHER) =
  Selective.Make_via_applicative
    (Applicative)
    (struct
      type nonrec 'a t = 'a t

      module Either = E

      let pure = pure

      let select e f =
        let open Either in
        value_of @@ map_left f e
    end)

module Monad = Monad.Make_via_bind (struct
  type nonrec 'a t = 'a t

  let return = pure

  let bind f = f
end)

let eq f a b = f a b

let pp pp' formater a = Format.fprintf formater "Identity (%a)" pp' a
