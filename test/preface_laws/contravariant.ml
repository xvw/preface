open Preface_qcheck

module Preserve_identity
    (F : Preface_specs.Contravariant.CORE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "map id = id"

  type input = X.t F.t
  type output = input

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.contramap (fun x -> x) x
  let right x = (fun x -> x) x
end)

module Preserve_morphism
    (F : Preface_specs.Contravariant.CORE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "contramap (f % g) = contramap g % contramap f"

  type input = X.t F.t * (Y.t -> Z.t) QCheck.fun_ * (Z.t -> X.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    triple (A.arbitrary X.arbitrary)
      (fun1 Y.observable Z.arbitrary)
      (fun1 Z.observable X.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    Preface_core.Fun.Infix.(F.contramap (g % f) x)
  ;;

  let right (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    Preface_core.Fun.Infix.(F.contramap f % F.contramap g) x
  ;;
end)

module Cases
    (F : Preface_specs.Contravariant.CORE)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Id = Preserve_identity (F) (A) (T.A)
  module Morphsim = Preserve_morphism (F) (A) (T.A) (T.B) (T.C)

  let cases n =
    [ Id.test n; Morphsim.test n ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
