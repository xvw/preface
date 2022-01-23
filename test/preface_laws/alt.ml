open Preface_qcheck

module Semigroup_cases
    (F : Preface_specs.ALT)
    (A : Model.T1 with type 'a t = 'a F.t)
    (T : Sample.PACKAGE) =
  Semigroup.Cases
    (Preface_make.Semigroup.From_alt (F) (T.A))
       (struct
         type t = T.A.t F.t

         let equal = A.equal T.A.equal
         let arbitrary = A.arbitrary T.A.arbitrary
         let observable = A.observable T.A.observable
       end)
