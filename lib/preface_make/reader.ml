module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Env : Preface_specs.Types.T0) =
struct
  type env = Env.t

  type 'a monad = 'a Monad.t

  type 'a t = env -> 'a monad

  let run reader_m env = reader_m env

  let ask = Monad.return

  let local f reader x = reader (f x)

  let reader f = f
end

module Functor (F : Preface_specs.FUNCTOR) (Env : Preface_specs.Types.T0) =
Functor.Via_map (struct
  type 'a t = Env.t -> 'a F.t

  let map f reader x = (F.map f) (reader x)
end)

module Applicative
    (A : Preface_specs.APPLICATIVE)
    (Env : Preface_specs.Types.T0) =
Applicative.Via_apply (struct
  type 'a t = Env.t -> 'a A.t

  let pure x = Fun.const (A.pure x)

  let apply reader_f reader_v x = A.apply (reader_f x) (reader_v x)
end)

module Alternative
    (A : Preface_specs.ALTERNATIVE)
    (Env : Preface_specs.Types.T0) =
  Alternative.Over_applicative
    (Applicative (A) (Env))
       (struct
         type 'a t = Env.t -> 'a A.t

         let neutral _ = A.neutral

         let combine reader_l reader_r r = A.combine (reader_l r) (reader_r r)
       end)

module Monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) =
Monad.Via_bind (struct
  type 'a t = Env.t -> 'a M.t

  let return x = Fun.const (M.return x)

  let bind f reader r = M.bind (fun a -> (f a) r) (reader r)
end)

module Monad_plus (M : Preface_specs.MONAD_PLUS) (Env : Preface_specs.Types.T0) =
  Monad_plus.Over_monad
    (Monad (M) (Env))
       (struct
         type 'a t = Env.t -> 'a M.t

         let neutral _ = M.neutral

         let combine reader_l reader_r r = M.combine (reader_l r) (reader_r r)
       end)

module Over_monad (M : Preface_specs.MONAD) (Env : Preface_specs.Types.T0) =
struct
  include Core_over_monad (M) (Env)
  module Monad = Monad (M) (Env)
  include Monad
end
