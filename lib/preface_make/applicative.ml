module Core_via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Core_via_pure_map_and_product (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_pure_and_apply
    (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Core_via_pure_and_apply (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_AND_APPLY
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_pure_and_lift2
    (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Core_via_pure_and_lift2 (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_AND_LIFT2
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Applicative.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_applicative.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_applicative.OPERATION
        with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Applicative.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_applicative.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_applicative.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_applicative.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Applicative.OPERATION with type 'a t := 'a Operation.t )
      end) :
      Preface_specs.Indexed_applicative.INFIX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Applicative.CORE)
    (Operation : Preface_specs.Applicative.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Applicative.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Applicative.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_applicative.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Core : Preface_specs.Applicative.CORE with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Applicative.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Infix : Preface_specs.Applicative.INFIX with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Applicative.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_applicative.API with type ('a, _) t := 'a Core.t )
end

module Via_pure_map_and_product
    (Req : Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Via_pure_map_and_product (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_MAP_AND_PRODUCT
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.API with type ('a, _) t := 'a Req.t )
end

module Via_pure_and_apply (Req : Preface_specs.Applicative.WITH_PURE_AND_APPLY) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Via_pure_and_apply (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_AND_APPLY
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.API with type ('a, _) t := 'a Req.t )
end

module Via_pure_and_lift2 (Req : Preface_specs.Applicative.WITH_PURE_AND_LIFT2) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_applicative.Via_pure_and_lift2 (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req :
          Preface_specs.Applicative.WITH_PURE_AND_LIFT2
            with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_applicative.API with type ('a, _) t := 'a Req.t )
end

module From_monad (Monad : Preface_specs.MONAD) = struct
  include Via_pure_and_apply (struct
    type 'a t = 'a Monad.t

    let pure = Monad.return

    let apply fs xs =
      let open Monad.Syntax in
      let* f = fs in
      let* x = xs in
      pure (f x)
    ;;
  end)
end

module From_alternative (Alternative : Preface_specs.ALTERNATIVE) = Alternative

module Over_apply
    (Apply : Preface_specs.APPLY)
    (Req : Preface_specs.Applicative.WITH_PURE with type 'a t = 'a Apply.t) =
struct
  include Via_pure_and_apply (struct
    type 'a t = 'a Apply.t

    let pure = Req.pure
    let apply = Apply.apply
  end)
end

module Composition
    (F : Preface_specs.APPLICATIVE)
    (G : Preface_specs.APPLICATIVE) =
Via_pure_and_apply (struct
  type 'a t = 'a G.t F.t

  let pure x = F.pure (G.pure x)
  let apply f x = F.lift2 G.apply f x
end)

module From_arrow (A : Preface_specs.ARROW) = Via_pure_and_apply (struct
  type 'a t = (unit, 'a) A.t

  let pure x = A.arrow (fun _ -> x)
  let uncurry f (x, y) = f x y
  let apply f x = A.(f &&& x >>> arrow (uncurry Fun.id))
end)

module Product (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) =
Via_pure_and_apply (struct
  type 'a t = 'a F.t * 'a G.t

  let pure x = (F.pure x, G.pure x)
  let apply (f, g) (x, y) = (F.apply f x, G.apply g y)
end)

module Const (M : Preface_specs.Monoid.CORE) = struct
  type 'a t = Const of M.t

  include (
    Via_pure_and_apply (struct
      type nonrec 'a t = 'a t

      let pure _ = Const M.neutral
      let apply (Const f) (Const x) = Const (M.combine f x)
    end) :
      Preface_specs.APPLICATIVE with type 'a t := 'a t )

  let get (Const value) = value
end

module Index (F : Preface_specs.APPLICATIVE) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_applicative.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Applicative.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F : Preface_specs.Applicative.OPERATION with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Infix : Preface_specs.Applicative.INFIX with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Applicative.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_APPLICATIVE
        with type ('a, 'index) t := ('a, 'index) t )
end
