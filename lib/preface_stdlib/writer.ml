module Over (Tape : Preface_specs.MONOID) = struct
  include Preface_make.Writer.Over_monad (Identity.Monad) (Tape)
  module Functor = Preface_make.Writer.Functor (Identity.Functor) (Tape)
  module Invariant = Preface_make.Invariant.From_functor (Functor)

  module Applicative =
    Preface_make.Writer.Applicative (Identity.Applicative) (Tape)

  let run_identity x = Identity.extract (run x)
  let exec_identity x = Identity.extract (exec x)
end
