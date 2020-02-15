module Operation (Core : Preface_specs.Functor.CORE) =
Functor2.Operation (struct
  type (_, 'a) t = 'a Core.t

  include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
end)

module Infix
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t) =
  Functor2.Infix
    (struct
      type (_, 'a) t = 'a Core.t

      include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
    end)
    (struct
      type (_, 'a) t = 'a Operation.t

      include (
        Operation :
          Preface_specs.Functor.OPERATION with type 'a t := 'a Operation.t )
    end)

module Via
    (Core : Preface_specs.Functor.CORE)
    (Operation : Preface_specs.Functor.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Functor.INFIX with type 'a t = 'a Operation.t) =
  Functor2.Via
    (struct
      type (_, 'a) t = 'a Core.t

      include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
    end)
    (struct
      type (_, 'a) t = 'a Operation.t

      include (
        Operation :
          Preface_specs.Functor.OPERATION with type 'a t := 'a Operation.t )
    end)
    (struct
      type (_, 'a) t = 'a Operation.t

      include (Infix : Preface_specs.Functor.INFIX with type 'a t := 'a Infix.t)
    end)

module Via_map (Core : Preface_specs.Functor.CORE) = Functor2.Via_map (struct
  type (_, 'a) t = 'a Core.t

  include (Core : Preface_specs.Functor.CORE with type 'a t := 'a Core.t)
end)
