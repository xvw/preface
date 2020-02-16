module Core_via_map_and_product
    (Core : Preface_specs.Applicative2.CORE_WITH_MAP_AND_PRODUCT) =
Applicative3.Core_via_map_and_product (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core :
      Preface_specs.Applicative2.CORE_WITH_MAP_AND_PRODUCT
        with type ('a, 'b) t := ('a, 'b) Core.t )
end)

module Core_via_apply (Core : Preface_specs.Applicative2.CORE_WITH_APPLY) =
Applicative3.Core_via_apply (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core :
      Preface_specs.Applicative2.CORE_WITH_APPLY
        with type ('a, 'b) t := ('a, 'b) Core.t )
end)

module Operation (Core : Preface_specs.Applicative2.CORE) =
Applicative3.Operation (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core :
      Preface_specs.Applicative2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
end)

module Syntax (Core : Preface_specs.Applicative2.CORE) =
Applicative3.Syntax (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core :
      Preface_specs.Applicative2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
end)

module Infix
    (Core : Preface_specs.Applicative2.CORE)
    (Operation : Preface_specs.Applicative2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) =
  Applicative3.Infix
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Core.t

      include (
        Core :
          Preface_specs.Applicative2.CORE
            with type ('a, 'b) t := ('a, 'b) Core.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Operation.t

      include (
        Operation :
          Preface_specs.Applicative2.OPERATION
            with type ('a, 'b) t := ('a, 'b) Operation.t )
    end)

module Via
    (Core : Preface_specs.Applicative2.CORE)
    (Operation : Preface_specs.Applicative2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Applicative2.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t)
    (Syntax : Preface_specs.Applicative2.SYNTAX
                with type ('a, 'b) t = ('a, 'b) Infix.t) =
  Applicative3.Via
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Core.t

      include (
        Core :
          Preface_specs.Applicative2.CORE
            with type ('a, 'b) t := ('a, 'b) Core.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Operation.t

      include (
        Operation :
          Preface_specs.Applicative2.OPERATION
            with type ('a, 'b) t := ('a, 'b) Operation.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Infix.t

      include (
        Infix :
          Preface_specs.Applicative2.INFIX
            with type ('a, 'b) t := ('a, 'b) Infix.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Syntax.t

      include (
        Syntax :
          Preface_specs.Applicative2.SYNTAX
            with type ('a, 'b) t := ('a, 'b) Syntax.t )
    end)

module Via_map_and_product
    (Core_with_map_and_product : Preface_specs.Applicative2
                                 .CORE_WITH_MAP_AND_PRODUCT) =
Applicative3.Via_map_and_product (struct
  type (_, 'a, 'b) t = ('a, 'b) Core_with_map_and_product.t

  include (
    Core_with_map_and_product :
      Preface_specs.Applicative2.CORE_WITH_MAP_AND_PRODUCT
        with type ('a, 'b) t := ('a, 'b) Core_with_map_and_product.t )
end)

module Via_apply (Core_with_apply : Preface_specs.Applicative2.CORE_WITH_APPLY) =
Applicative3.Via_apply (struct
  type (_, 'a, 'b) t = ('a, 'b) Core_with_apply.t

  include (
    Core_with_apply :
      Preface_specs.Applicative2.CORE_WITH_APPLY
        with type ('a, 'b) t := ('a, 'b) Core_with_apply.t )
end)
