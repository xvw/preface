module Core_via_fold_right (Req : Preface_specs.Foldable.WITH_FOLD_RIGHT) =
struct
  type 'a t = 'a Req.t

  include (
    Indexed_foldable.Core_via_fold_right (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Foldable.WITH_FOLD_RIGHT with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_foldable.CORE with type ('a, _) t := 'a Req.t )
end

module Core_via_fold_map (Req : Preface_specs.Foldable.WITH_FOLD_MAP) = struct
  type 'a t = 'a Req.t

  include (
    Indexed_foldable.Core_via_fold_map (struct
      type ('a, 'index) t = 'a Req.t

      include (
        Req : Preface_specs.Foldable.WITH_FOLD_MAP with type 'a t := 'a Req.t )
    end) :
      Preface_specs.Indexed_foldable.CORE with type ('a, _) t := 'a Req.t )
end

module Operation (C : Preface_specs.Foldable.CORE) = struct
  type 'a t = 'a C.t

  include (
    Indexed_foldable.Operation (struct
      type ('a, 'index) t = 'a C.t

      include (C : Preface_specs.Foldable.CORE with type 'a t := 'a C.t)
    end) :
      Preface_specs.Indexed_foldable.OPERATION with type ('a, _) t := 'a C.t )
end

module Via
    (C : Preface_specs.Foldable.CORE)
    (O : Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t) =
struct
  include C
  include (O : Preface_specs.Foldable.OPERATION with type 'a t := 'a t)
end

module Via_fold_right (Req : Preface_specs.Foldable.WITH_FOLD_RIGHT) = struct
  module C = Core_via_fold_right (Req)
  module O = Operation (C)
  include Via (C) (O)
end

module Via_fold_map (Req : Preface_specs.Foldable.WITH_FOLD_MAP) = struct
  module C = Core_via_fold_map (Req)
  module O = Operation (C)
  include Via (C) (O)
end

module Composition (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) =
Via_fold_map (struct
  type 'a t = 'a G.t F.t

  let fold_map' neutral combine f x =
    F.fold_map' neutral combine (G.fold_map' neutral combine f) x
  ;;
end)

module Sum (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) = struct
  type 'a sum =
    | L of 'a F.t
    | R of 'a G.t

  include Via_fold_map (struct
    type 'a t = 'a sum

    let fold_map' neutral combine f = function
      | L x -> F.fold_map' neutral combine f x
      | R x -> G.fold_map' neutral combine f x
    ;;
  end)
end

module Product (F : Preface_specs.FOLDABLE) (G : Preface_specs.FOLDABLE) =
Via_fold_map (struct
  type 'a t = 'a F.t * 'a G.t

  let fold_map' neutral combine f (x, y) =
    combine (F.fold_map' neutral combine f x) (G.fold_map' neutral combine f y)
  ;;
end)

module Index (F : Preface_specs.FOLDABLE) = struct
  type ('a, 'index) t = 'a F.t

  include (
    Indexed_foldable.Via
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Foldable.CORE with type 'a t := 'a F.t)
      end)
      (struct
        type nonrec ('a, 'index) t = ('a, 'index) t

        include (F : Preface_specs.Foldable.OPERATION with type 'a t := 'a F.t)
      end) :
      Preface_specs.INDEXED_FOLDABLE with type ('a, 'index) t := ('a, 'index) t )
end
