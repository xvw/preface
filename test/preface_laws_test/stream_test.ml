open Preface_test
include Preface_stdlib.Stream

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "Stream"

  let arbitrary = Qcheck_helpers.Arbitrary.stream
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x = Obj.magic (take 15 x)
end

module Stream_functor =
  Functor_laws.Make_with_post_hook (Functor) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module Stream_applicative =
  Applicative_laws.Make_with_post_hook (Applicative) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module Stream_monad =
  Monad_laws.Make_with_post_hook (Monad) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

let cases =
  [ Stream_functor.cases; Stream_applicative.cases; Stream_monad.cases ]
;;
