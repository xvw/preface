open Preface_test
include Preface_stdlib.Continuation

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Continuation"

  let arbitrary = Qcheck_helpers.Arbitrary.continuation
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x = Obj.magic (x.run Preface_core.Fun.id)
end

module Continuation_functor =
  Functor_laws.Make_with_post_hook (Functor) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Continuation_applicative =
  Applicative_laws.Make_with_post_hook (Applicative) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Continuation_monad =
  Monad_laws.Make_with_post_hook (Monad) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

let cases =
  [
    Continuation_functor.cases
  ; Continuation_applicative.cases
  ; Continuation_monad.cases
  ]
;;
