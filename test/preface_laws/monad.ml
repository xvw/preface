open Preface_qcheck

module Join_map
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "join % map return = join % return = id"

  type input = X.t F.t
  type output = X.t F.t * X.t F.t

  let arbitrary = A.arbitrary X.arbitrary

  let equal (a, b) (c, d) =
    let eq = A.equal X.equal in
    eq a b && eq b c && eq c d && eq d a
  ;;

  let left x = Preface_core.Fun.(F.((join % map return) x), id x)

  let right x =
    let open Preface_core.Fun in
    F.((join % return) x, id x)
  ;;
end)

module Natural_transformation
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "map f % return = return f"

  type input = X.t * (X.t -> Y.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary = QCheck.(pair X.arbitrary (fun1 X.observable Y.arbitrary))
  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    let open Preface_core.Fun in
    F.((map f % return) x)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    let open Preface_core.Fun in
    F.((return % f) x)
  ;;
end)

module Left_identity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "return x >>= f = f x"

  type input = X.t * (X.t -> Y.t F.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (fun1 X.observable (A.arbitrary Y.arbitrary)))
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    F.(return x >>= f)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Right_identity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "x >>= return = x"

  type input = X.t F.t
  type output = X.t F.t

  let arbitrary = A.arbitrary X.arbitrary
  let equal = A.equal X.equal
  let left x = F.(x >>= return)
  let right x = x
end)

module Kleisli_left_identity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "return >=> f = f"

  type input = X.t * (X.t -> Y.t F.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (fun1 X.observable (A.arbitrary Y.arbitrary)))
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    F.((return >=> f) x)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Kleisli_right_identity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "f >=> return = f"

  type input = X.t * (X.t -> Y.t F.t) QCheck.fun_
  type output = Y.t F.t

  let arbitrary =
    QCheck.(pair X.arbitrary (fun1 X.observable (A.arbitrary Y.arbitrary)))
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    F.((f >=> return) x)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    f x
  ;;
end)

module Cases
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Bind = Bind.Cases (F) (A) (T)
  module Join_map = Join_map (F) (A) (T.A)
  module Natural_transformation = Natural_transformation (F) (A) (T.A) (T.B)
  module Left_identity = Left_identity (F) (A) (T.A) (T.B)
  module Right_identity = Right_identity (F) (A) (T.A)
  module Kleisli_left_identity = Kleisli_left_identity (F) (A) (T.A) (T.B)
  module Kleisli_right_identity = Kleisli_right_identity (F) (A) (T.A) (T.B)

  let cases n =
    Bind.cases n
    @ ( [
          Join_map.test n
        ; Natural_transformation.test n
        ; Left_identity.test n
        ; Right_identity.test n
        ; Kleisli_left_identity.test n
        ; Kleisli_right_identity.test n
        ]
      |> Stdlib.List.map QCheck_alcotest.to_alcotest )
  ;;
end
