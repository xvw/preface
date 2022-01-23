open Preface_qcheck

module Preserve_identity
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Make.Test (struct
  let name = "extend extract = id"
  let arbitrary = A.arbitrary X.arbitrary

  type input = X.t F.t
  type output = X.t F.t

  let equal = A.equal X.equal
  let left x = F.(extend extract) x
  let right x = Fun.id x
end)

module Extract_extend
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Make.Test (struct
  let name = "extract % extend f = f"

  let arbitrary =
    let open QCheck in
    pair
      (fun1 (A.observable X.observable) Y.arbitrary)
      (A.arbitrary X.arbitrary)
  ;;

  type input = (X.t F.t -> Y.t) QCheck.fun_ * X.t F.t
  type output = Y.t

  let equal = Y.equal

  let left (f', x) =
    let open Preface_stdlib.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    F.(extract % extend f) x
  ;;

  let right (f', x) =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Extend_extend
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Make.Test (struct
  let name = "extend f % extend g = extend (f % extend g)"

  let arbitrary =
    let open QCheck in
    triple
      (fun1 (A.observable Y.observable) Z.arbitrary)
      (fun1 (A.observable X.observable) Y.arbitrary)
      (A.arbitrary X.arbitrary)
  ;;

  type input =
    (Y.t F.t -> Z.t) QCheck.fun_ * (X.t F.t -> Y.t) QCheck.fun_ * X.t F.t

  type output = Z.t F.t

  let equal = A.equal Z.equal

  let left (f', g', x) =
    let open Preface_stdlib.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.(extend f % extend g) x
  ;;

  let right (f', g', x) =
    let open Preface_stdlib.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    F.(extend (f % extend g)) x
  ;;
end)

module Infix_extract_extend
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Make.Test (struct
  let name = "f =>= extract = f"

  let arbitrary =
    let open QCheck in
    pair
      (fun1 (A.observable X.observable) Y.arbitrary)
      (A.arbitrary X.arbitrary)
  ;;

  type input = (X.t F.t -> Y.t) QCheck.fun_ * X.t F.t
  type output = Y.t

  let equal = Y.equal

  let left (f', x) =
    let f = QCheck.Fn.apply f' in
    F.(f =>= extract) x
  ;;

  let right (f', x) =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Rev_infix_extract_extend
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Make.Test (struct
  let name = "extract =>= f = f"

  let arbitrary =
    let open QCheck in
    pair
      (fun1 (A.observable X.observable) Y.arbitrary)
      (A.arbitrary X.arbitrary)
  ;;

  type input = (X.t F.t -> Y.t) QCheck.fun_ * X.t F.t
  type output = Y.t

  let equal = Y.equal

  let left (f', x) =
    let f = QCheck.Fn.apply f' in
    F.(extract =>= f) x
  ;;

  let right (f', x) =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Infix_extend_triple
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Make.Test (struct
  let name = "(f =>= g) =>= h = f =>= (g =>= h)"

  let arbitrary =
    let open QCheck in
    quad
      (fun1 (A.observable W.observable) X.arbitrary)
      (fun1 (A.observable X.observable) Y.arbitrary)
      (fun1 (A.observable Y.observable) Z.arbitrary)
      (A.arbitrary W.arbitrary)
  ;;

  type input =
    (W.t F.t -> X.t) QCheck.fun_
    * (X.t F.t -> Y.t) QCheck.fun_
    * (Y.t F.t -> Z.t) QCheck.fun_
    * W.t F.t

  type output = Z.t

  let equal = Z.equal

  let left (f', g', h', x) =
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    F.(f =>= g =>= h) x
  ;;

  let right (f', g', h', x) =
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    F.(f =>= g =>= h) x
  ;;
end)

module Duplicate_preserve_identity
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Make.Test (struct
  let name = "extract % duplicate = id"
  let arbitrary = A.arbitrary X.arbitrary

  type input = X.t F.t
  type output = X.t F.t

  let equal = A.equal X.equal
  let left x = Preface_stdlib.Fun.Infix.(F.(extract % duplicate) x)
  let right x = Fun.id x
end)

module Map_duplicate_preserve_identity
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Make.Test (struct
  let name = "map extract % duplicate = id"
  let arbitrary = A.arbitrary X.arbitrary

  type input = X.t F.t
  type output = X.t F.t

  let equal = A.equal X.equal
  let left x = Preface_stdlib.Fun.Infix.(F.(map extract % duplicate) x)
  let right x = Fun.id x
end)

module Map_duplicate_duplicate
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Make.Test (struct
  let name = "duplicate % duplicate = map duplicate % duplicate"
  let arbitrary = A.arbitrary X.arbitrary

  type input = X.t F.t
  type output = X.t F.t F.t F.t

  let equal = A.equal (A.equal (A.equal X.equal))
  let left x = Preface_stdlib.Fun.Infix.(F.(duplicate % duplicate) x)
  let right x = Preface_stdlib.Fun.Infix.(F.(map duplicate % duplicate) x)
end)

module Extend_map_duplicate
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Make.Test (struct
  let name = "extend f = map f % duplicate"

  let arbitrary =
    let open QCheck in
    pair
      (fun1 (A.observable X.observable) Y.arbitrary)
      (A.arbitrary X.arbitrary)
  ;;

  type input = (X.t F.t -> Y.t) QCheck.fun_ * X.t F.t
  type output = Y.t F.t

  let equal = A.equal Y.equal

  let left (f', x) =
    let f = QCheck.Fn.apply f' in
    F.(extend f) x
  ;;

  let right (f', x) =
    let f = QCheck.Fn.apply f' in
    Preface_core.Fun.Infix.(F.(map f % duplicate) x)
  ;;
end)

module Duplicate_extend_id
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Make.Test (struct
  let name = "duplicate = extend id"
  let arbitrary = A.arbitrary X.arbitrary

  type input = X.t F.t
  type output = X.t F.t F.t

  let equal = A.equal (A.equal X.equal)
  let left x = F.duplicate x
  let right x = (F.extend Fun.id) x
end)

module Map_extend_extract
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Make.Test (struct
  let name = "map f = extend (f % extract)"

  let arbitrary =
    QCheck.(pair (fun1 X.observable Y.arbitrary) (A.arbitrary X.arbitrary))
  ;;

  type input = (X.t -> Y.t) QCheck.fun_ * X.t F.t
  type output = Y.t F.t

  let equal = A.equal Y.equal

  let left (f', x) =
    let f = QCheck.Fn.apply f' in
    F.map f x
  ;;

  let right (f', x) =
    let open Preface_core.Fun.Infix in
    let f = QCheck.Fn.apply f' in
    F.(extend (f % extract)) x
  ;;
end)

module Cases
    (F : Preface_specs.COMONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Preserve_identity = Preserve_identity (F) (A) (T.A)
  module Extract_extend = Extract_extend (F) (A) (T.A) (T.B)
  module Extend_extend = Extend_extend (F) (A) (T.A) (T.B) (T.C)
  module Infix_extract_extend = Infix_extract_extend (F) (A) (T.A) (T.B)
  module Rev_infix_extract_extend = Rev_infix_extract_extend (F) (A) (T.A) (T.B)

  module Infix_extend_triple =
    Infix_extend_triple (F) (A) (T.A) (T.B) (T.C) (T.D)

  module Duplicate_preserve_identity = Duplicate_preserve_identity (F) (A) (T.A)

  module Map_duplicate_preserve_identity =
    Map_duplicate_preserve_identity (F) (A) (T.A)

  module Map_duplicate_duplicate = Map_duplicate_duplicate (F) (A) (T.A)
  module Extend_map_duplicate = Extend_map_duplicate (F) (A) (T.A) (T.B)
  module Duplicate_extend_id = Duplicate_extend_id (F) (A) (T.A)
  module Map_extend_extract = Map_extend_extract (F) (A) (T.A) (T.B)

  let cases n =
    [
      Preserve_identity.test n
    ; Extract_extend.test n
    ; Extend_extend.test n
    ; Infix_extract_extend.test n
    ; Rev_infix_extract_extend.test n
    ; Infix_extend_triple.test n
    ; Duplicate_preserve_identity.test n
    ; Map_duplicate_preserve_identity.test n
    ; Map_duplicate_duplicate.test n
    ; Extend_map_duplicate.test n
    ; Duplicate_extend_id.test n
    ; Map_extend_extract.test n
    ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
