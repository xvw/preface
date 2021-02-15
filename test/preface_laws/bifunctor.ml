open Preface_qcheck

module Perserve_identity
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "bimap id id = id"

  type input = (X.t, Y.t) F.t

  type output = input

  let arbitrary = A.arbitrary X.arbitrary Y.arbitrary

  let equal = A.equal X.equal Y.equal

  let left x = F.bimap (fun x -> x) (fun x -> x) x

  let right x = (fun x -> x) x
end)

module Perserve_first
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "fst id = id"

  type input = (X.t, Y.t) F.t

  type output = input

  let arbitrary = A.arbitrary X.arbitrary Y.arbitrary

  let equal = A.equal X.equal Y.equal

  let left x = F.fst (fun x -> x) x

  let right x = (fun x -> x) x
end)

module Perserve_second
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "snd id = id"

  type input = (X.t, Y.t) F.t

  type output = input

  let arbitrary = A.arbitrary X.arbitrary Y.arbitrary

  let equal = A.equal X.equal Y.equal

  let left x = F.snd (fun x -> x) x

  let right x = (fun x -> x) x
end)

module Bimap_fst_snd
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "bimap f g = (fst f) % (snd g)"

  type input =
    (W.t, X.t) F.t * (W.t -> Y.t) QCheck.fun_ * (X.t -> Z.t) QCheck.fun_

  type output = (Y.t, Z.t) F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary W.arbitrary X.arbitrary)
      (fun1 W.observable Y.arbitrary)
      (fun1 X.observable Z.arbitrary)
  ;;

  let equal = A.equal Y.equal Z.equal

  let left (x, f', g') =
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.bimap f g x
  ;;

  let right (x, f', g') =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.(fst f % snd g) x
  ;;
end)

module Bimap_parametricity
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (U : Model.T0)
    (V : Model.T0)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "bimap (f % g) (h % i) = (bimap f h) % (bimap g i)"

  type input =
    (U.t, V.t) F.t
    * ((W.t -> X.t) QCheck.fun_ * (U.t -> W.t) QCheck.fun_)
    * ((Y.t -> Z.t) QCheck.fun_ * (V.t -> Y.t) QCheck.fun_)

  type output = (X.t, Z.t) F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary U.arbitrary V.arbitrary)
      (pair (fun1 W.observable X.arbitrary) (fun1 U.observable W.arbitrary))
      (pair (fun1 Y.observable Z.arbitrary) (fun1 V.observable Y.arbitrary))
  ;;

  let equal = A.equal X.equal Z.equal

  let left (x, (f', g'), (h', i')) =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    let i = QCheck.Fn.apply i' in
    F.(bimap (f % g) (h % i)) x
  ;;

  let right (x, (f', g'), (h', i')) =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    let i = QCheck.Fn.apply i' in
    F.(bimap f h % bimap g i) x
  ;;
end)

module Fst_parametricity
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "fst (f % g) = (fst f) % (fst g)"

  type input =
    (W.t, X.t) F.t * (Y.t -> Z.t) QCheck.fun_ * (W.t -> Y.t) QCheck.fun_

  type output = (Z.t, X.t) F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary W.arbitrary X.arbitrary)
      (fun1 Y.observable Z.arbitrary)
      (fun1 W.observable Y.arbitrary)
  ;;

  let equal = A.equal Z.equal X.equal

  let left (x, f', g') =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.fst (f % g) x
  ;;

  let right (x, f', g') =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.(fst f % fst g) x
  ;;
end)

module Snd_parametricity
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "snd (f % g) = (snd f) % (snd g)"

  type input =
    (W.t, X.t) F.t * (Y.t -> Z.t) QCheck.fun_ * (X.t -> Y.t) QCheck.fun_

  type output = (W.t, Z.t) F.t

  let arbitrary =
    let open QCheck in
    triple
      (A.arbitrary W.arbitrary X.arbitrary)
      (fun1 Y.observable Z.arbitrary)
      (fun1 X.observable Y.arbitrary)
  ;;

  let equal = A.equal W.equal Z.equal

  let left (x, f', g') =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.snd (f % g) x
  ;;

  let right (x, f', g') =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.(snd f % snd g) x
  ;;
end)

module Cases
    (F : Preface_specs.BIFUNCTOR)
    (A : Model.T2 with type ('a, 'b) t = ('a, 'b) F.t)
    (T : Sample.PACKAGE) =
struct
  module Id = Perserve_identity (F) (A) (T.A) (T.B)
  module First = Perserve_first (F) (A) (T.A) (T.B)
  module Second = Perserve_second (F) (A) (T.A) (T.B)
  module Bimap_fst_snd = Bimap_fst_snd (F) (A) (T.A) (T.B) (T.C) (T.D)
  module Bimap_parametricity =
    Bimap_parametricity (F) (A) (T.A) (T.B) (T.C) (T.D) (T.E) (T.F)
  module Fst_parametricity = Fst_parametricity (F) (A) (T.A) (T.B) (T.C) (T.D)
  module Snd_parametricity = Snd_parametricity (F) (A) (T.A) (T.B) (T.C) (T.D)

  let cases n =
    [
      Id.test n
    ; First.test n
    ; Second.test n
    ; Bimap_fst_snd.test n
    ; Bimap_parametricity.test n
    ; Fst_parametricity.test n
    ; Snd_parametricity.test n
    ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
