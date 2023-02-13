module Core_via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Core_via_map_and_duplicate (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Core_via_extend (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Comonad.WITH_EXTEND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_cokleisli_composition
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Core_via_cokleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.CORE with type ('a, _) t := 'a Req.t )
end

module Syntax (Core : Preface_specs.Comonad.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_comonad.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Comonad.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_comonad.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Operation (Core : Preface_specs.Comonad.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_comonad.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Comonad.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_comonad.OPERATION with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_comonad.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Comonad.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Operation.t

        include (
          Operation :
            Preface_specs.Comonad.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_comonad.INFIX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Comonad.CORE)
    (Operation : Preface_specs.Comonad.OPERATION with type 'a t := 'a Core.t)
    (Infix : Preface_specs.Comonad.INFIX with type 'a t := 'a Core.t)
    (Syntax : Preface_specs.Comonad.SYNTAX with type 'a t := 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_comonad.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Comonad.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Comonad.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Infix : Preface_specs.Comonad.INFIX with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Comonad.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_comonad.API with type ('a, _) t := 'a Core.t )
end

module Via_map_and_duplicate
    (Req : Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Via_map_and_duplicate (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Comonad.WITH_MAP_AND_DUPLICATE
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.API with type ('a, _) t := 'a Req.t )
end

module Via_extend (Req : Preface_specs.Comonad.WITH_EXTEND) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Via_extend (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Comonad.WITH_EXTEND with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.API with type ('a, _) t := 'a Req.t )
end

module Via_cokleisli_composition
    (Req : Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_comonad.Via_cokleisli_composition (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Comonad.WITH_COKLEISLI_COMPOSITION
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_comonad.API with type ('a, _) t := 'a Req.t )
end

module Index (F : Preface_specs.COMONAD) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_comonad.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Comonad.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Comonad.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F.Infix : Preface_specs.Comonad.INFIX with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Comonad.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_COMONAD with type ('a, 'index) t := ('a, 'index) t )
end
