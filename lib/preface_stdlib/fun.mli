(** Work with functions.contents

    {1 Capabilities}

    - {!val:Category}
    - {!val:Arrow} *)

(** {1 Type} *)

type ('a, 'b) t = 'a -> 'b

(** {1 Implementation} *)

module Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) t
(** {2 Category API} *)

module Arrow : Preface_specs.ARROW with type ('a, 'b) t = ('a, 'b) t
(** {2 Arrow API} *)

(** {1 API} *)

include module type of Preface_core.Fun
