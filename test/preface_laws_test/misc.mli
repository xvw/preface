module Sum : Preface.Specs.MONOID with type t = int
module Prod : Preface.Specs.MONOID with type t = int

val cases : count:int -> (string * unit Alcotest.test_case list) list
