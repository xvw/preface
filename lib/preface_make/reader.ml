module Over (T : Preface_specs.Types.T0) = struct
  open Preface_core.Fun

  type env = T.t

  type 'a t = env -> 'a

  let pure = constant

  let map = ( <% )

  module Functor = Functor.Via_map (struct
    type nonrec 'a t = 'a t

    let map = map
  end)

  module Applicative = Applicative.Via_apply (struct
    type nonrec 'a t = 'a t

    let pure = pure

    let apply mf ma s = map (mf s) ma s
  end)

  module Monad = Monad.Via_bind (struct
    type nonrec 'a t = 'a t

    let return = pure

    let bind f ma s = map f ma s s
  end)

  (** {2 Helpers} *)

  let run = id

  let ask = id

  let local = ( %> )

  let reader = id
end

(*
module Over_update(T : Preface_specs.Types.T0) = struct
  module Unit = Monoid.Via_combine_and_neutral(struct
    type t = unit
    let neutral = ()
    let combine () () = ()
  end)

  module Update = Update.Over(T)(Unit)(struct
    let right _ s = s
  end)

  type env = Update.env

  type 'a t = 'a Update.t

  module Functor = Update.Functor

  module Applicative = Update.Applicative

  module Monad = Update.Monad

  let run a = failwith "TODO"

  let ask = fun e -> e,()

  let local f a = fun e -> a (f e),()

  let reader f = fun e -> (f e), ()
   (** Build a reader from a function *)

end
*)
