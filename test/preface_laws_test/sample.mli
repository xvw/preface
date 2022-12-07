(** Data sampling modules for use in PBT. *)

module Int : Preface_qcheck.Model.T0 with type t = int
module Float : Preface_qcheck.Model.T0 with type t = float
module String : Preface_qcheck.Model.T0 with type t = string
module Char : Preface_qcheck.Model.T0 with type t = char
module Bool : Preface_qcheck.Model.T0 with type t = bool
