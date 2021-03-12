(** Work with functions.contents

    {1 Capabilities}

    - {!val:Profunctor}
    - {!val:Strong}
    - {!val:Choice}
    - {!val:Category}
    - {!val:Arrow}
    - {!val:Arrow_choice}
    - {!val:Arrow_apply} *)

(** {1 Type} *)

type ('a, 'b) t = 'a -> 'b

(** {1 Implementation} *)

module Profunctor : Preface_specs.PROFUNCTOR with type ('a, 'b) t = ('a, 'b) t
(** {2 Profunctor API} *)

module Strong : Preface_specs.STRONG with type ('a, 'b) t = ('a, 'b) t
(** {2 Strong Profunctor API} *)

module Choice : Preface_specs.CHOICE with type ('a, 'b) t = ('a, 'b) t
(** {2 Choice Profunctor API} *)

module Closed : Preface_specs.CLOSED with type ('a, 'b) t = ('a, 'b) t
(** {2 Closed Profunctor API} *)

module Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) t
(** {2 Category API} *)

module Arrow : Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) t
(** {2 Arrow API} *)

module Arrow_choice :
  Preface_specs.ARROW_CHOICE with type ('a, 'b) t = ('a, 'b) t
(** {2 Arrow Choice API} *)

module Arrow_apply : Preface_specs.ARROW_APPLY with type ('a, 'b) t = ('a, 'b) t
(** {2 Arrow Apply API} *)

(** {1 API} *)

include module type of Preface_core.Fun
