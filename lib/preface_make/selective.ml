module Core_over_functor_via_select
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT
             with type 'a t = 'a Functor.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_selective.Core_over_functor_via_select
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Functor :
            Preface_specs.Functor.WITH_MAP with type 'a t := 'a Functor.t )
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req :
            Preface_specs.Selective.WITH_PURE_AND_SELECT
              with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_selective.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_functor_via_branch
    (Functor : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH
             with type 'a t = 'a Functor.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_selective.Core_over_functor_via_branch
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Functor :
            Preface_specs.Functor.WITH_MAP with type 'a t := 'a Functor.t )
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req :
            Preface_specs.Selective.WITH_PURE_AND_BRANCH
              with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_selective.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_applicative_via_select
    (Applicative : Preface_specs.Applicative.CORE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a Applicative.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_selective.Core_over_applicative_via_select
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Applicative :
            Preface_specs.Applicative.CORE with type 'a t := 'a Applicative.t )
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req : Preface_specs.Selective.WITH_SELECT with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_selective.CORE with type ('a, _) t := 'a Req.t )
end

module Core_over_applicative_via_branch
    (Applicative : Preface_specs.Applicative.CORE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a Applicative.t) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_selective.Core_over_applicative_via_branch
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Applicative :
            Preface_specs.Applicative.CORE with type 'a t := 'a Applicative.t )
      end)
      (struct
        type ('a, 'index) t = 'a Req.t

        include (
          Req : Preface_specs.Selective.WITH_BRANCH with type 'a t := 'a Req.t )
      end) :
      Preface_specs.Indexed_selective.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (Core : Preface_specs.Selective.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_selective.Operation (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Selective.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_selective.OPERATION with type ('a, _) t := 'a Core.t )
end

module Infix
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_selective.Infix
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Selective.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Selective.OPERATION with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_selective.INFIX with type ('a, _) t := 'a Core.t )
end

module Syntax (Core : Preface_specs.Selective.CORE) = struct
  type 'a t = 'a Core.t

  include (
    Indexed_selective.Syntax (struct
      type ('a, 'index) t = 'a Core.t

      include (Core : Preface_specs.Selective.CORE with type 'a t := 'a Core.t)
    end) :
      Preface_specs.Indexed_selective.SYNTAX with type ('a, _) t := 'a Core.t )
end

module Via
    (Core : Preface_specs.Selective.CORE)
    (Operation : Preface_specs.Selective.OPERATION with type 'a t = 'a Core.t)
    (Infix : Preface_specs.Selective.INFIX with type 'a t = 'a Core.t)
    (Syntax : Preface_specs.Selective.SYNTAX with type 'a t = 'a Core.t) =
struct
  type 'a t = 'a Core.t

  include (
    Indexed_selective.Via
      (struct
        type ('a, 'index) t = 'a Core.t

        include (Core : Preface_specs.Selective.CORE with type 'a t := 'a Core.t)
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Operation :
            Preface_specs.Selective.OPERATION with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Infix : Preface_specs.Selective.INFIX with type 'a t := 'a Core.t )
      end)
      (struct
        type ('a, 'index) t = 'a Core.t

        include (
          Syntax : Preface_specs.Selective.SYNTAX with type 'a t := 'a Core.t )
      end) :
      Preface_specs.Indexed_selective.API with type ('a, _) t := 'a Core.t )
end

module Over_functor_via_select
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Selective.WITH_PURE_AND_SELECT with type 'a t = 'a F.t) =
struct
  type 'a t = 'a F.t

  include (
    Indexed_selective.Over_functor_via_select
      (struct
        type ('a, 'index) t = 'a F.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a F.t

        include (
          Req :
            Preface_specs.Selective.WITH_PURE_AND_SELECT
              with type 'a t := 'a F.t )
      end) :
      Preface_specs.Indexed_selective.API with type ('a, _) t := 'a Req.t )
end

module Over_functor_via_branch
    (F : Preface_specs.Functor.WITH_MAP)
    (Req : Preface_specs.Selective.WITH_PURE_AND_BRANCH with type 'a t = 'a F.t) =
struct
  type 'a t = 'a F.t

  include (
    Indexed_selective.Over_functor_via_branch
      (struct
        type ('a, 'index) t = 'a F.t

        include (F : Preface_specs.Functor.WITH_MAP with type 'a t := 'a F.t)
      end)
      (struct
        type ('a, 'index) t = 'a F.t

        include (
          Req :
            Preface_specs.Selective.WITH_PURE_AND_BRANCH
              with type 'a t := 'a F.t )
      end) :
      Preface_specs.Indexed_selective.API with type ('a, _) t := 'a Req.t )
end

module Over_applicative_via_select
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_SELECT with type 'a t = 'a A.t) =
struct
  type 'a t = 'a A.t

  include (
    Indexed_selective.Over_applicative_via_select
      (struct
        type ('a, 'index) t = 'a A.t

        include (A : Preface_specs.APPLICATIVE with type 'a t := 'a A.t)
      end)
      (struct
        type ('a, 'index) t = 'a A.t

        include (
          Req : Preface_specs.Selective.WITH_SELECT with type 'a t := 'a A.t )
      end) :
      Preface_specs.Indexed_selective.API with type ('a, _) t := 'a Req.t )
end

module Over_applicative_via_branch
    (A : Preface_specs.APPLICATIVE)
    (Req : Preface_specs.Selective.WITH_BRANCH with type 'a t = 'a A.t) =
struct
  type 'a t = 'a A.t

  include (
    Indexed_selective.Over_applicative_via_branch
      (struct
        type ('a, 'index) t = 'a A.t

        include (A : Preface_specs.APPLICATIVE with type 'a t := 'a A.t)
      end)
      (struct
        type ('a, 'index) t = 'a A.t

        include (
          Req : Preface_specs.Selective.WITH_BRANCH with type 'a t := 'a A.t )
      end) :
      Preface_specs.Indexed_selective.API with type ('a, _) t := 'a Req.t )
end

module Select_from_monad (Monad : Preface_specs.Monad.CORE) = struct
  type 'a t = 'a Monad.t

  include (
    Indexed_selective.Select_from_monad (struct
      type ('a, 'index) t = 'a Monad.t

      include (Monad : Preface_specs.Monad.CORE with type 'a t := 'a Monad.t)
    end) :
      Preface_specs.Indexed_selective.WITH_SELECT
        with type ('a, _) t := 'a Monad.t )
end

module From_arrow_choice (A : Preface_specs.ARROW_CHOICE) =
  Over_applicative_via_select
    (Applicative.From_arrow
       (A))
       (struct
         type 'a t = (unit, 'a) A.t

         let to_arrow f =
           A.(arrow (fun x -> ((), x)) >>> fst f >>> arrow (fun (f, x) -> f x))
         ;;

         let select x y = A.(x >>> (to_arrow y ||| return ()))
       end)

module Composition (F : Preface_specs.APPLICATIVE) (G : Preface_specs.SELECTIVE) =
  Over_applicative_via_select
    (Applicative.Composition (F) (G))
       (struct
         type 'a t = 'a G.t F.t

         let select x y = F.apply (F.map G.select x) y
       end)

module Product (F : Preface_specs.SELECTIVE) (G : Preface_specs.SELECTIVE) =
  Over_applicative_via_select
    (Applicative.Product (F) (G))
       (struct
         type 'a t = 'a F.t * 'a G.t

         let select (x1, y1) (x2, y2) = (F.select x1 x2, G.select y1 y2)
       end)

module Const (M : Preface_specs.Monoid.CORE) = struct
  module App = Applicative.Const (M)

  type 'a t = 'a App.t = Const of M.t

  include (
    Over_applicative_via_select
      (App)
      (struct
        type nonrec 'a t = 'a t

        let select (Const x) (Const y) = Const (M.combine x y)
      end) :
        Preface_specs.SELECTIVE with type 'a t := 'a t )

  let get (Const value) = value
end

module Index (F : Preface_specs.SELECTIVE) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_selective.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Selective.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Selective.OPERATION with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Infix : Preface_specs.Selective.INFIX with type 'a t := 'a F.t )
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (
          F.Syntax : Preface_specs.Selective.SYNTAX with type 'a t := 'a F.t )
      end) :
      Preface_specs.INDEXED_SELECTIVE with type ('a, 'index) t := ('a, 'index) t )
end
