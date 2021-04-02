module Over (Env : Preface_specs.Types.T0) = struct
  include Preface_make.Reader.Over_monad (Identity.Monad) (Env)
  module Functor = Preface_make.Reader.Functor (Identity.Functor) (Env)
  module Applicative =
    Preface_make.Reader.Applicative (Identity.Applicative) (Env)

  let run_identity reader env = Identity.extract (run reader env)
end
