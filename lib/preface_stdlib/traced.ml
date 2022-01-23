module Over (Tape : Preface_specs.MONOID) = struct
  include Preface_make.Traced.Over_comonad (Identity.Comonad) (Tape)

  let traced f = Identity.pure f
  let run_identity f x = (Identity.extract f) x

  module Functor = Preface_make.Traced.Functor (Identity.Functor) (Tape)
  module Invariant = Preface_make.Invariant.From_functor (Functor)
end
