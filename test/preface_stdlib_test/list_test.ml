module Functor_test = Support.Functor (struct
  include Preface_stdlib.List
  include Functor
end)

module Applicative_test = Support.Applicative (struct
  include Preface_stdlib.List
  include Applicative
end)

module Monad_test = Support.Monad (struct
  include Preface_stdlib.List
  include Monad
end)

let test_cases =
  [
    ("List Functor", Functor_test.cases)
  ; ("List Applicative", Applicative_test.cases)
  ; ("List Monad", Monad_test.cases)
  ]
;;
