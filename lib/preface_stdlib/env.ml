module Over (Env : Preface_specs.Types.T0) = struct
  include Preface_make.Env.Over_comonad (Identity.Comonad) (Env)

  let env e x = (e, Identity.pure x)
  let run_identity (e, x) = (e, Identity.extract x)

  module Functor = Preface_make.Env.Functor (Identity.Functor) (Env)
  module Invariant = Preface_make.Invariant.From_functor (Functor)
end
