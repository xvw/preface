module Functor_test = Support.Functor (struct
  include Preface_stdlib.Identity
  include Functor
end)

module Applicative_test = Support.Applicative (struct
  include Preface_stdlib.Identity
  include Applicative
end)

module Monad_test = Support.Monad (struct
  include Preface_stdlib.Identity
  include Monad
end)

let test_cases =
  [
    ("Identity Functor", Functor_test.cases)
  ; ("Identity Applicative", Applicative_test.cases)
  ; ("Identity Monad", Monad_test.cases)
  ]
;;
