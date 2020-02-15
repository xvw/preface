module Operation (Core : Preface_specs.Functor2.CORE) =
Functor3.Operation (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core : Preface_specs.Functor2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
end)

module Infix
    (Core : Preface_specs.Functor2.CORE)
    (Operation : Preface_specs.Functor2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t) =
  Functor3.Infix
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Core.t

      include (
        Core :
          Preface_specs.Functor2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Operation.t

      include (
        Operation :
          Preface_specs.Functor2.OPERATION
            with type ('a, 'b) t := ('a, 'b) Operation.t )
    end)

module Via
    (Core : Preface_specs.Functor2.CORE)
    (Operation : Preface_specs.Functor2.OPERATION
                   with type ('a, 'b) t = ('a, 'b) Core.t)
    (Infix : Preface_specs.Functor2.INFIX
               with type ('a, 'b) t = ('a, 'b) Operation.t) =
  Functor3.Via
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Core.t

      include (
        Core :
          Preface_specs.Functor2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Operation.t

      include (
        Operation :
          Preface_specs.Functor2.OPERATION
            with type ('a, 'b) t := ('a, 'b) Operation.t )
    end)
    (struct
      type (_, 'a, 'b) t = ('a, 'b) Infix.t

      include (
        Infix :
          Preface_specs.Functor2.INFIX with type ('a, 'b) t := ('a, 'b) Infix.t )
    end)

module Via_map (Core : Preface_specs.Functor2.CORE) = Functor3.Via_map (struct
  type (_, 'a, 'b) t = ('a, 'b) Core.t

  include (
    Core : Preface_specs.Functor2.CORE with type ('a, 'b) t := ('a, 'b) Core.t )
end)
