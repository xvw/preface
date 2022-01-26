open Preface_qcheck

module Preserve_identity
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "pure id <*> x = x"

  type input = X.t F.t
  type output = input

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.(pure (fun x -> x) <*> x)
  let right x = x
end)

module Homomorphism
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "pure f <*> pure x = pure (f x)"

  type input = X.t * (X.t -> Y.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary = QCheck.(pair X.arbitrary (fun1 X.observable Y.arbitrary))
  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    F.(pure f <*> pure x)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    F.pure (f x)
  ;;
end)

module Interchange
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "f <*> pure x = pure ((|>) x) <*> f"

  type input = X.t * (X.t -> Y.t) QCheck.fun_ F.t
  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (A.arbitrary @@ fun1 X.observable Y.arbitrary))
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = F.map QCheck.Fn.apply f' in
    F.(f <*> pure x)
  ;;

  let right (x, f') =
    let f = F.map QCheck.Fn.apply f' in
    F.(pure (( |> ) x) <*> f)
  ;;
end)

module Composition
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "pure ( % ) <*> u <*> v <*> w = u <*> (v <*> w)"

  type input =
    (X.t -> Z.t) QCheck.fun_ F.t * (Y.t -> X.t) QCheck.fun_ F.t * Y.t F.t

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary (fun1 X.observable Z.arbitrary))
      (A.arbitrary (fun1 Y.observable X.arbitrary))
      (A.arbitrary Y.arbitrary)
  ;;

  let equal = A.equal Z.equal

  let left (u', v', w) =
    let u = F.map QCheck.Fn.apply u' in
    let v = F.map QCheck.Fn.apply v' in
    let open Preface_core.Fun.Infix in
    F.(pure ( % ) <*> u <*> v <*> w)
  ;;

  let right (u', v', w) =
    let u = F.map QCheck.Fn.apply u' in
    let v = F.map QCheck.Fn.apply v' in
    F.(u <*> (v <*> w))
  ;;
end)

module Map_additional
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "fmap f x = pure f <*> x"

  type input = X.t F.t * (X.t -> Y.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair (A.arbitrary X.arbitrary) (fun1 X.observable Y.arbitrary))
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    F.map f x
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    F.(pure f <*> x)
  ;;
end)

module Ignore_left
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "u *> v = (id <$ u) <*> v"

  type input = X.t F.t
  type output = input

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.(pure () *> x)
  let right x = F.((fun x -> x) <$ pure () <*> x)
end)

module Ignore_right
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "u <* v = lift2 const u v"

  type input = X.t F.t
  type output = input

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.(x <* pure ())
  let right x = F.(lift2 Preface_core.Fun.const x (pure ()))
end)

module Cases
    (F : Preface_specs.APPLICATIVE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Apply = Apply.Cases (F) (A) (T)
  module Id = Preserve_identity (F) (A) (T.A)
  module Homomorphism = Homomorphism (F) (A) (T.A) (T.B)
  module Interchange = Interchange (F) (A) (T.A) (T.B)
  module Composition = Composition (F) (A) (T.A) (T.B) (T.C)
  module Map = Map_additional (F) (A) (T.A) (T.B)

  let cases n =
    (Apply.cases n)
    @ ([
      Id.test n
    ; Homomorphism.test n
    ; Interchange.test n
    ; Composition.test n
    ; Map.test n
    ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest)
  ;;
end
