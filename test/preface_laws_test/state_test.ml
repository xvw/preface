open Preface_test

include Preface_stdlib.State.Over (struct
  type t = int
end)

module Requirement = struct
  type nonrec 'a t = 'a t

  let suite_name = "State"

  let arbitrary = Qcheck_helpers.Arbitrary.state
end

module Hook = struct
  type nonrec 'a t = 'a t

  let apply x =
    Obj.magic
      (let open Monad in
      let v =
        let* _ = x in
        get
      in
      fst (v 42))
  ;;
end

module State_functor =
  Functor_laws.Make_with_post_hook (Functor) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.String)
module State_applicative =
  Applicative_laws.Make_with_post_hook (Applicative) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
module State_monad =
  Monad_laws.Make_with_post_hook (Monad) (Requirement) (Hook)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)
    (Qcheck_helpers.Sample.Int)
    (Qcheck_helpers.Sample.String)

let cases = [ State_functor.cases; State_applicative.cases; State_monad.cases ]
