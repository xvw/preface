open Preface_core.Fun

module Operation (Core : Preface_specs.Bifunctor.CORE) :
  Preface_specs.Bifunctor.OPERATION with type ('a, 'b) t = ('a, 'b) Core.t =
struct
  type ('a, 'b) t = ('a, 'b) Core.t

  let map f x = Core.fst f x

  let replace_fst value x = Core.fst (constant value) x

  let replace_snd value x = Core.snd (constant value) x
end
