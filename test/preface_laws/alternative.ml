open Preface_qcheck

module Apply_right_distributivity
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(f <|> g) <*> a = (f <*> a) <|> (g <*> a)"

  type input =
    (X.t -> Y.t) QCheck.fun_ F.t * (X.t -> Y.t) QCheck.fun_ F.t * X.t F.t

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary (fun1 X.observable Y.arbitrary))
      (A.arbitrary (fun1 X.observable Y.arbitrary))
      (A.arbitrary X.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (f', g', a) =
    let f = F.(QCheck.Fn.apply <$> f') in
    let g = F.(QCheck.Fn.apply <$> g') in
    F.(f <|> g <*> a)
  ;;

  let right (f', g', a) =
    let f = F.(QCheck.Fn.apply <$> f') in
    let g = F.(QCheck.Fn.apply <$> g') in
    F.(f <*> a <|> (g <*> a))
  ;;
end)

module Right_absorption
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "neutral <*> a = neutral"

  type input = X.t F.t
  type output = X.t F.t

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.(neutral <*> x)
  let right _ = F.neutral
end)

module Monoidal_behaviour
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
  Monoid.Cases
    (Preface_make.Monoid.From_alternative (F) (X))
       (struct
         type t = X.t F.t

         let equal = A.equal X.equal
         let arbitrary = A.arbitrary X.arbitrary
         let observable = A.observable X.observable
       end)

module Cases_for_monoidal
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monoidal = Monoidal_behaviour (F) (A) (T.A)

  let cases n = Monoidal.cases n
end

module Cases_for_apply_right_distribituvity
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Right = Apply_right_distributivity (F) (A) (T.A) (T.B)

  let cases n = [ Right.test n ] |> Stdlib.List.map QCheck_alcotest.to_alcotest
end

module Cases_for_right_absorption
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Right = Right_absorption (F) (A) (T.A)

  let cases n = [ Right.test n ] |> Stdlib.List.map QCheck_alcotest.to_alcotest
end

module Cases
    (F : Preface_specs.ALTERNATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Monoidal = Monoidal_behaviour (F) (A) (T.A)
  module Right_apply = Apply_right_distributivity (F) (A) (T.A) (T.B)
  module Right_absorption = Right_absorption (F) (A) (T.A)

  let cases n =
    Monoidal.cases n
    @ ( [ Right_absorption.test n; Right_apply.test n ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end
