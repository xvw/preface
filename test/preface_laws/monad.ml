open Preface_qcheck

module Join_map_1
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "join % join = join % map join"

  type input = X.t F.t F.t F.t

  type output = X.t F.t

  let arbitrary = A.arbitrary (A.arbitrary (A.arbitrary X.arbitrary))

  let equal = A.equal X.equal

  let left x =
    let open Preface_core.Fun.Infix in
    F.((join % join) x)
  ;;

  let right x =
    let open Preface_core.Fun.Infix in
    F.((join % map join) x)
  ;;
end)

module Join_map_2
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

module Natural_transformation_1
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "map id = id"

  type input = X.t F.t

  type output = X.t F.t

  let arbitrary = A.arbitrary X.arbitrary

  let equal = A.equal X.equal

  let left x =
    let open Preface_core.Fun in
    F.(map id x)
  ;;

  let right x = Preface_core.Fun.(id x)
end)

module Natural_transformation_2
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "map (f % g) = map f % map g"

  type input = X.t F.t * (Z.t -> Y.t) QCheck.fun_ * (X.t -> Z.t) QCheck.fun_

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    triple (A.arbitrary X.arbitrary)
      (fun1 Z.observable Y.arbitrary)
      (fun1 X.observable Z.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    let open Preface_core.Fun in
    F.(map (f % g) x)
  ;;

  let right (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    let open Preface_core.Fun in
    F.((map f % map g) x)
  ;;
end)

module Natural_transformation_3
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "map f % join = join % map (map f)"

  type input = X.t F.t F.t * (X.t -> Y.t) QCheck.fun_

  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    pair (A.arbitrary (A.arbitrary X.arbitrary)) (fun1 X.observable Y.arbitrary)
  ;;

  let equal = A.equal Y.equal

  let left (x, f') =
    let f = QCheck.Fn.apply f' in
    let open Preface_core.Fun in
    F.((map f % join) x)
  ;;

  let right (x, f') =
    let f = QCheck.Fn.apply f' in
    let open Preface_core.Fun in
    F.((join % map (map f)) x)
  ;;
end)

module Natural_transformation_4
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

module Associativity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(x >>= f) >>= g = x >>= (fun y -> f y >>= g)"

  type input =
    X.t F.t * (X.t -> Y.t F.t) QCheck.fun_ * (Y.t -> Z.t F.t) QCheck.fun_

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    triple (A.arbitrary X.arbitrary)
      (fun1 X.observable (A.arbitrary Y.arbitrary))
      (fun1 Y.observable (A.arbitrary Z.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    F.(x >>= f >>= g)
  ;;

  let right (x, f', g') =
    let f = QCheck.Fn.apply f'
    and g = QCheck.Fn.apply g' in
    F.(x >>= (fun y -> f y >>= g))
  ;;
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

module Kleisli_associativity
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (W : Model.T0)
    (X : Model.T0)
    (Y : Model.T0)
    (Z : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "(f >=> g) >=> h = f >=> (g >=> h)"

  type input =
    W.t
    * (W.t -> X.t F.t) QCheck.fun_
    * (X.t -> Y.t F.t) QCheck.fun_
    * (Y.t -> Z.t F.t) QCheck.fun_

  type output = Z.t F.t

  let arbitrary =
    let open QCheck in
    quad W.arbitrary
      (fun1 W.observable (A.arbitrary X.arbitrary))
      (fun1 X.observable (A.arbitrary Y.arbitrary))
      (fun1 Y.observable (A.arbitrary Z.arbitrary))
  ;;

  let equal = A.equal Z.equal

  let left (x, f', g', h') =
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    F.((f >=> g >=> h) x)
  ;;

  let right (x, f', g', h') =
    let f = QCheck.Fn.apply f' in
    let g = QCheck.Fn.apply g' in
    let h = QCheck.Fn.apply h' in
    F.((f >=> (g >=> h)) x)
  ;;
end)

module Cases
    (F : Preface_specs.MONAD)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Join_map_1 = Join_map_1 (F) (A) (T.A)
  module Join_map_2 = Join_map_2 (F) (A) (T.A)
  module Natural_transformation_1 = Natural_transformation_1 (F) (A) (T.A)
  module Natural_transformation_2 =
    Natural_transformation_2 (F) (A) (T.A) (T.B) (T.C)
  module Natural_transformation_3 = Natural_transformation_3 (F) (A) (T.A) (T.B)
  module Natural_transformation_4 = Natural_transformation_4 (F) (A) (T.A) (T.B)
  module Left_identity = Left_identity (F) (A) (T.A) (T.B)
  module Right_identity = Right_identity (F) (A) (T.A)
  module Associativity = Associativity (F) (A) (T.A) (T.B) (T.C)
  module Kleisli_left_identity = Kleisli_left_identity (F) (A) (T.A) (T.B)
  module Kleisli_right_identity = Kleisli_right_identity (F) (A) (T.A) (T.B)
  module Kleisli_associativity =
    Kleisli_associativity (F) (A) (T.A) (T.B) (T.C) (T.D)

  let cases n =
    [
      Join_map_1.test n
    ; Join_map_2.test n
    ; Natural_transformation_1.test n
    ; Natural_transformation_2.test n
    ; Natural_transformation_3.test n
    ; Natural_transformation_4.test n
    ; Left_identity.test n
    ; Right_identity.test n
    ; Associativity.test n
    ; Kleisli_left_identity.test n
    ; Kleisli_right_identity.test n
    ; Kleisli_associativity.test n
    ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
