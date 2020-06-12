(** Work with functions.contents

    {1 Capabilities}

    - {!val:Category} *)

(** {1 Type} *)

type ('a, 'b) t = 'a -> 'b

(** {1 Implementation} *)

module Category : Preface_specs.CATEGORY with type ('a, 'b) t = ('a, 'b) t
(** {2 Category API} *)

(** {1 API} *)

include module type of Preface_core.Fun
