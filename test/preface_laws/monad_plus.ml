open Preface_qcheck

module Left_absorption
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "neutral >>= x = neutral"

  type input = (X.t -> Y.t F.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary = QCheck.fun1 X.observable (A.arbitrary Y.arbitrary)
  let equal = A.equal Y.equal

  let left f' =
    let f = QCheck.Fn.apply f' in
    F.(neutral >>= f)
  ;;

  let right _ = F.neutral
end)

module Bind_left_distributivity
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(a <|> b) >>= f = (a >>= f) <|> (b >>= f)"

  type input = X.t F.t * X.t F.t * (X.t -> Y.t F.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    triple (A.arbitrary X.arbitrary) (A.arbitrary X.arbitrary)
      (fun1 X.observable (A.arbitrary Y.arbitrary))
  ;;

  let equal = A.equal Y.equal

  let left (a, b, f') =
    let f = QCheck.Fn.apply f' in
    F.(a <|> b >>= f)
  ;;

  let right (a, b, f') =
    let f = QCheck.Fn.apply f' in
    F.(a >>= f <|> (b >>= f))
  ;;
end)

module Left_catch
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(return a) <|> b = return a"

  type input = X.t * X.t F.t
  type output = X.t F.t

  let arbitrary = QCheck.(pair X.arbitrary (A.arbitrary X.arbitrary))
  let equal = A.equal X.equal
  let left (a, b) = F.(return a <|> b)
  let right (a, _) = F.return a
end)

module Monoidal_behaviour
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
  Monoid.Cases
    (Preface_make.Monoid.From_monad_plus (F) (X))
       (struct
         type t = X.t F.t

         let equal = A.equal X.equal
         let arbitrary = A.arbitrary X.arbitrary
         let observable = A.observable X.observable
       end)

module Cases_for_monoidal
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monad = Monad.Cases (F) (A) (T)
  module Monoidal = Monoidal_behaviour (F) (A) (T.A)

  let cases n = Monad.cases n @ Monoidal.cases n
end

module Cases_for_left_absorption
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monad = Monad.Cases (F) (A) (T)
  module Left = Left_absorption (F) (A) (T.A) (T.B)

  let cases n =
    Monad.cases n
    @ ([ Left.test n ] |> Stdlib.List.map QCheck_alcotest.to_alcotest)
  ;;
end

module Cases_for_bind_left_distributivity
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monad = Monad.Cases (F) (A) (T)
  module Left = Bind_left_distributivity (F) (A) (T.A) (T.B)

  let cases n =
    Monad.cases n
    @ ([ Left.test n ] |> Stdlib.List.map QCheck_alcotest.to_alcotest)
  ;;
end

module Cases_for_left_catch
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monad = Monad.Cases (F) (A) (T)
  module Left = Left_catch (F) (A) (T.A)

  let cases n =
    Monad.cases n
    @ ([ Left.test n ] |> Stdlib.List.map QCheck_alcotest.to_alcotest)
  ;;
end

module Cases
    (F : Preface_specs.MONAD_PLUS)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monad = Monad.Cases (F) (A) (T)
  module Monoidal = Monoidal_behaviour (F) (A) (T.A)
  module Left_catch = Left_catch (F) (A) (T.A)
  module Left_absorption = Left_absorption (F) (A) (T.A) (T.B)
  module Left_distribution = Bind_left_distributivity (F) (A) (T.A) (T.B)

  let cases n =
    Monad.cases n
    @ Monoidal.cases n
    @ ( [ Left_catch.test n; Left_absorption.test n; Left_distribution.test n ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end
