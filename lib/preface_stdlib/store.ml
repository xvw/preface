module Over (Store : Preface_specs.Types.T0) = struct
  include Preface_make.Store.Over_comonad (Identity.Comonad) (Store)

  let store f s = (Identity.pure f, s)

  let run_identity c =
    let f, s = run c in
    (Identity.extract f, s)
  ;;

  module Functor = Preface_make.Store.Functor (Identity.Functor) (Store)
  module Invariant = Preface_make.Invariant.From_functor (Functor)
end
