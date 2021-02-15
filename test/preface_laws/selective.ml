open Preface_qcheck
module Either = Preface_stdlib.Either

let either l r = Arbitrary.either l r

module Identity_case
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "x <*? pure id = Either.case id id <$> x"

  type input = (X.t, X.t) Either.t F.t

  type output = X.t F.t

  let arbitrary = A.arbitrary (either X.arbitrary X.arbitrary)

  let equal = A.equal X.equal

  let left x = F.(x <*? pure (fun x -> x))

  let right x = F.(Either.case (fun x -> x) (fun x -> x) <$> x)
end)

module Distributive
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)"

  type input =
    (X.t, Y.t) Either.t
    * (X.t -> Y.t) QCheck.fun_ F.t
    * (X.t -> Y.t) QCheck.fun_ F.t

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (either X.arbitrary Y.arbitrary)
      (A.arbitrary (fun1 X.observable Y.arbitrary))
      (A.arbitrary (fun1 X.observable Y.arbitrary))
  ;;

  let equal = A.equal Y.equal

  let left (x, y', z') =
    let y = F.map QCheck.Fn.apply y' in
    let z = F.map QCheck.Fn.apply z' in
    F.(pure x <*? replace () y *> z)
  ;;

  let right (x, y', z') =
    let y = F.map QCheck.Fn.apply y' in
    let z = F.map QCheck.Fn.apply z' in
    F.(replace () (pure x <*? y) *> (pure x <*? z))
  ;;
end)

module Associative
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "x <*? (y <*? z) = (f <$> x) <*? (g <$> y) <*? (h <$> z)"

  type input =
    (Y.t, Z.t) Either.t F.t
    * (X.t, (Y.t -> Z.t) QCheck.fun_) Either.t F.t
    * (X.t -> Y.t -> Z.t) QCheck.fun_ F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary (either Y.arbitrary Z.arbitrary))
      (A.arbitrary (either X.arbitrary (fun1 Y.observable Z.arbitrary)))
      (A.arbitrary (fun2 X.observable Y.observable Z.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (x, y', z') =
    let y = F.(Either.map_right QCheck.Fn.apply <$> y') in
    let z = F.(QCheck.Fn.apply <$> z') in
    F.(x <*? (y <*? z))
  ;;

  let right (x, y', z') =
    let f a = Either.map_right (fun x -> Either.Right x) a
    and g x a = Either.Bifunctor.bimap (fun x -> (x, a)) (fun f -> f a) x
    and h x (a, b) = x a b in
    let y = F.(Either.map_right QCheck.Fn.apply <$> y') in
    let z = F.(QCheck.Fn.apply <$> z') in
    F.(f <$> x <*? (g <$> y) <*? (h <$> z))
  ;;
end)

module Theorem1
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name =
    "(f <$> select x y) = (select (Bifunctor.snd f <$> x) (((%) f) <$> y))"
  ;;

  type input =
    (X.t -> Z.t) QCheck.fun_
    * (Y.t, X.t) Either.t F.t
    * (Y.t -> X.t) QCheck.fun_ F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (fun1 X.observable Z.arbitrary)
      (A.arbitrary (either Y.arbitrary X.arbitrary))
      (A.arbitrary (fun1 Y.observable X.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (f', x, y') =
    let f = QCheck.Fn.apply f'
    and y = F.map QCheck.Fn.apply y' in
    F.(f <$> select x y)
  ;;

  let right (f', x, y') =
    let f = QCheck.Fn.apply f'
    and y = F.map QCheck.Fn.apply y' in
    let open Preface_core.Fun in
    F.(select (Either.Bifunctor.snd f <$> x) (( % ) f <$> y))
  ;;
end)

module Theorem2
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(select (Bifunctor.fst f <$> x) y) = (select x ((%>) f) <$> y))"

  type input =
    (X.t -> Y.t) QCheck.fun_
    * (X.t, Z.t) Either.t F.t
    * (Y.t -> Z.t) QCheck.fun_ F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (fun1 X.observable Y.arbitrary)
      (A.arbitrary (either X.arbitrary Z.arbitrary))
      (A.arbitrary (fun1 Y.observable Z.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (f', x, y') =
    let f = QCheck.Fn.apply f'
    and y = F.map QCheck.Fn.apply y' in
    F.(select (Either.Bifunctor.fst f <$> x) y)
  ;;

  let right (f', x, y') =
    let f = QCheck.Fn.apply f'
    and y = F.map QCheck.Fn.apply y' in
    let open Preface_core.Fun in
    F.(select x (( %> ) f <$> y))
  ;;
end)

module Theorem3
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name =
    "(select x (f <$> y)) = (select (Bifunctor.fst (flip f) <$> x) ((|>) <$> \
     y))"
  ;;

  type input =
    (X.t -> Y.t -> Z.t) QCheck.fun_ * (Y.t, Z.t) Either.t F.t * X.t F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (fun2 X.observable Y.observable Z.arbitrary)
      (A.arbitrary (either Y.arbitrary Z.arbitrary))
      (A.arbitrary X.arbitrary)
  ;;

  let equal = A.equal Z.equal

  let left (f', x, y) =
    let f = QCheck.Fn.apply f' in
    F.(select x (f <$> y))
  ;;

  let right (f', x, y) =
    let f = QCheck.Fn.apply f' in
    let open Preface_core.Fun in
    F.(select (Either.Bifunctor.fst (flip f) <$> x) (( |> ) <$> y))
  ;;
end)

module Theorem4
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(x <*? pure y) = (Either.case y id <$> x)"

  type input = (X.t, Y.t) Either.t F.t * (X.t -> Y.t) QCheck.fun_

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    pair
      (A.arbitrary (either X.arbitrary Y.arbitrary))
      (fun1 X.observable Y.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (x, y') =
    let y = QCheck.Fn.apply y' in
    F.(x <*? pure y)
  ;;

  let right (x, y') =
    let y = QCheck.Fn.apply y' in
    let open Preface_core.Fun in
    F.(Either.case y id <$> x)
  ;;
end)

module Pure_right
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(pure (Right x) <*? y) = pure x"

  type input = X.t * (Y.t -> X.t) QCheck.fun_ F.t

  type output = X.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (A.arbitrary (fun1 Y.observable X.arbitrary)))
  ;;

  let equal = A.equal X.equal

  let left (x, y') =
    let y = F.map QCheck.Fn.apply y' in
    F.(pure (Either.Right x) <*? y)
  ;;

  let right (x, _) = F.(pure x)
end)

module Pure_left
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(pure (Left x) <*? y) = ((|>) x) <$> y"

  type input = X.t * (X.t -> Y.t) QCheck.fun_ F.t

  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (A.arbitrary (fun1 X.observable Y.arbitrary)))
  ;;

  let equal = A.equal Y.equal

  let left (x, y') =
    let y = F.map QCheck.Fn.apply y' in
    F.(pure (Either.Left x) <*? y)
  ;;

  let right (x, y') =
    let y = F.map QCheck.Fn.apply y' in
    F.(( |> ) x <$> y)
  ;;
end)

module Theorem5
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "f <*> g = apply f g"

  type input = (X.t -> Y.t) QCheck.fun_ F.t * X.t F.t

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    pair (A.arbitrary (fun1 X.observable Y.arbitrary)) (A.arbitrary X.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (f', g) =
    let f = F.map QCheck.Fn.apply f' in
    F.(f <*> g)
  ;;

  let right (f', g) =
    let f = F.map QCheck.Fn.apply f' in
    F.(apply f g)
  ;;
end)

module Theorem6
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(x *> (y <*? z)) = ((x *> y) <*? z)"

  type input = X.t F.t * (Y.t, Z.t) Either.t F.t * (Y.t -> Z.t) QCheck.fun_ F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple (A.arbitrary X.arbitrary)
      (A.arbitrary (either Y.arbitrary Z.arbitrary))
      (A.arbitrary (fun1 Y.observable Z.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (x, y, z') =
    let z = F.map QCheck.Fn.apply z' in
    let open F in
    let b = y <*? z in
    let a = ignore <$> x in
    a *> b
  ;;

  let right (x, y, z') =
    let z = F.map QCheck.Fn.apply z' in
    let open F in
    let a = ignore <$> x in
    let b = a *> y in
    b <*? z
  ;;
end)

module Cases
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Applicative = Applicative.Cases (F) (A) (T)
  module Identity_case = Identity_case (F) (A) (T.A)
  module Distributive = Distributive (F) (A) (T.A) (T.B)
  module Associative = Associative (F) (A) (T.A) (T.B) (T.C)
  module Theorem1 = Theorem1 (F) (A) (T.A) (T.B) (T.C)
  module Theorem2 = Theorem2 (F) (A) (T.A) (T.B) (T.C)
  module Theorem3 = Theorem3 (F) (A) (T.A) (T.B) (T.C)
  module Theorem4 = Theorem4 (F) (A) (T.A) (T.B)
  module Pure_right = Pure_right (F) (A) (T.A) (T.B)
  module Pure_left = Pure_left (F) (A) (T.A) (T.B)

  let cases n =
    Applicative.cases n
    @ ( [
          Identity_case.test n
        ; Distributive.test n
        ; Associative.test n
        ; Theorem1.test n
        ; Theorem2.test n
        ; Theorem3.test n
        ; Theorem4.test n
        ; Pure_right.test n
        ; Pure_left.test n
        ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end

module Rigid_cases
    (F : Preface_specs.SELECTIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Base_cases = Cases (F) (A) (T)
  module Theorem5 = Theorem5 (F) (A) (T.A) (T.B)
  module Theorem6 = Theorem6 (F) (A) (T.A) (T.B) (T.C)

  let cases n =
    Base_cases.cases n
    @ ( [ Theorem5.test n; Theorem6.test n ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end
