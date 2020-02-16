module Core_via_map_and_product
    (Core : Preface_specs.Applicative.CORE_WITH_MAP_AND_PRODUCT) =
Applicative2.Core_via_map_and_product (struct
  type (_, 'a) t = 'a Core.t

  include (
    Core :
      Preface_specs.Applicative.CORE_WITH_MAP_AND_PRODUCT
        with type 'a t := 'a Core.t )
end)

module Core_via_apply (Core : Preface_specs.Applicative.CORE_WITH_APPLY) =
Applicative2.Core_via_apply (struct
  type (_, 'a) t = 'a Core.t

  include (
    Core : Preface_specs.Applicative.CORE_WITH_APPLY with type 'a t := 'a Core.t )
end)

module Operation (Core : Preface_specs.Applicative.CORE) =
Applicative2.Operation (struct
  type (_, 'a) t = 'a Core.t

  include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
end)

module Syntax (Core : Preface_specs.Applicative.CORE) =
Applicative2.Syntax (struct
  type (_, 'a) t = 'a Core.t

  include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
end)

module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) =
  Applicative2.Infix
    (struct
      type (_, 'a) t = 'a Core.t

      include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
    end)
    (struct
      type (_, 'a) t = 'a Operation.t

      include (
        Operation :
          Preface_specs.Applicative.OPERATION with type 'a t := 'a Operation.t )
    end)

module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Operation.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Infix.t) =
  Applicative2.Via
    (struct
      type (_, 'a) t = 'a Core.t

      include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
    end)
    (struct
      type (_, 'a) t = 'a Operation.t

      include (
        Operation :
          Preface_specs.Applicative.OPERATION with type 'a t := 'a Operation.t )
    end)
    (struct
      type (_, 'a) t = 'a Infix.t

      include (
        Infix : Preface_specs.Applicative.INFIX with type 'a t := 'a Infix.t )
    end)
    (struct
      type (_, 'a) t = 'a Syntax.t

      include (
        Syntax : Preface_specs.Applicative.SYNTAX with type 'a t := 'a Syntax.t )
    end)

module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative
                                 .CORE_WITH_MAP_AND_PRODUCT) =
Applicative2.Via_map_and_product (struct
  type (_, 'a) t = 'a Core_with_map_and_product.t

  include (
    Core_with_map_and_product :
      Preface_specs.Applicative.CORE_WITH_MAP_AND_PRODUCT
        with type 'a t := 'a Core_with_map_and_product.t )
end)

module Via_apply (Core_with_apply : Preface_specs.Applicative.CORE_WITH_APPLY) =
Applicative2.Via_apply (struct
  type (_, 'a) t = 'a Core_with_apply.t

  include (
    Core_with_apply :
      Preface_specs.Applicative.CORE_WITH_APPLY
        with type 'a t := 'a Core_with_apply.t )
end)
