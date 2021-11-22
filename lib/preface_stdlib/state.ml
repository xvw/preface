module Over (State : Preface_specs.Types.T0) = struct
  include Preface_make.State.Over_monad (Identity.Monad) (State)

  let exec_identity m s = Identity.extract (exec m s)

  let eval_identity m s = Identity.extract (eval m s)

  let run_identity m s = Identity.extract (run m s)

  module Functor = Preface_make.State.Functor (Identity.Functor) (State)
  module Applicative = Preface_make.State.Applicative (Identity.Monad) (State)
  module Invariant = Preface_make.Invariant.From_functor (Functor)
end
