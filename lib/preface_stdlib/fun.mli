(** Implementation for function ['a -> 'b]. *)

(** {1 Type} *)

type ('a, 'b) t = 'a -> 'b

(** {1 Implementation} *)

(** {2 Profunctor} *)

module Profunctor : Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) t

(** {2 Strong Profunctor} *)

module Strong : Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) t

(** {2 Choice Profunctor} *)

module Choice : Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) t

(** {2 Closed Profunctor} *)

module Closed : Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) t

(** {2 Category} *)

module Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) t

(** {2 Arrow} *)

module Arrow : Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) t

(** {2 Arrow Choice} *)

module Arrow_choice :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) t

(** {2 Arrow Apply} *)

module Arrow_apply : Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) t

(** {1 Additional functions}

    Additional functions to facilitate practical work with [Fun.t]. *)

include module type of Preface_core.Fun
(** @inline *)
