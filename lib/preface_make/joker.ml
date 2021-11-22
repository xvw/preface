module To_bifunctor (F : Preface_specs.Functor.CORE) =
Bifunctor.Via_bimap (struct
  type ('a, 'b) t = 'b F.t

  let bimap _ f x = F.map f x
end)

module To_profunctor (F : Preface_specs.Functor.CORE) =
Profunctor.Via_dimap (struct
  type ('a, 'b) t = 'b F.t

  let dimap _ f x = F.map f x
end)
