open Preface_core.Fun.Infix

module Make (R : sig type t end) = struct
    type 'a t = ('a -> R.t) -> R.t

    let pure c = fun k -> k c

    let map f c = fun k -> c @@ f %> k

    module Functor = Preface_make.Functor.Via_map (struct
        type nonrec 'a t = 'a t

        let map = map
    end)

    module Applicative = Preface_make.Applicative.Via_map_and_product (struct
        type nonrec 'a t = 'a t

        let pure = pure

        let map = map

        let product ca cb = fun k -> ca (fun a -> cb (fun b -> k (a,b)))
    end)

    module Monad = Preface_make.Monad.Via_map_and_join (struct
        type nonrec 'a t = 'a t

        let return = pure

        let map = map

        let join c = fun k -> c (fun c' -> c' k)
    end)

end
