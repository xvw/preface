(** Wrapper around each component of [Preface]. *)

(** {2 Signatures} *)

module Specs = Preface_specs

(** {2 Constructions} *)

module Make = Preface_make

(** {2 Standard Library} *)

module Fun = Preface_core.Fun
include Preface_stdlib
