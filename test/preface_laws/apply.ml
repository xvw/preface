open Preface_qcheck

module Ignore_left
    (F : Preface_specs.APPLY)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "u *> v = (id <$ u) <*> v"

  type input = unit F.t * Y.t F.t
  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    pair (A.arbitrary unit) (A.arbitrary Y.arbitrary)
  ;;

  let equal = A.equal Y.equal
  let left (x, y) = F.(x *> y)
  let right (x, y) = F.(Preface_core.Fun.id <$ x <*> y)
end)

module Ignore_right
    (F : Preface_specs.APPLY)
    (A : Model.T1 with type 'a t = 'a F.t)
    (X : Model.T0)
    (Y : Model.T0) =
Preface_qcheck.Make.Test (struct
  let name = "u <* v = lift2 const u v"

  type input = unit F.t * Y.t F.t
  type output = Y.t F.t

  let arbitrary =
    let open QCheck in
    pair (A.arbitrary unit) (A.arbitrary Y.arbitrary)
  ;;

  let equal = A.equal Y.equal
  let left (x, y) = F.(y <* x)
  let right (x, y) = F.(lift2 Preface_core.Fun.const y x)
end)

module Preserve_functor_identity (A : Preface_specs.APPLY) =
  Functor.Preserve_identity (A)

module Preserve_functor_morphism (A : Preface_specs.APPLY) =
  Functor.Preserve_morphism (A)

module Cases
    (F : Preface_specs.APPLY)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
struct
  module Ignore_left = Ignore_left (F) (A) (T.H) (T.A)
  module Ignore_right = Ignore_right (F) (A) (T.H) (T.A)
  module Functor_id = Preserve_functor_identity (F) (A) (T.A)
  module Functor_morphism = Preserve_functor_morphism (F) (A) (T.A) (T.B) (T.C)

  let cases n =
    [
      Ignore_left.test n
    ; Ignore_right.test n
    ; Functor_id.test n
    ; Functor_morphism.test n
    ]
    |> Stdlib.List.map QCheck_alcotest.to_alcotest
  ;;
end
