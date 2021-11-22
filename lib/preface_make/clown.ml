module To_bifunctor (F : Preface_specs.Functor.CORE) =
Bifunctor.Via_bimap (struct
  type ('a, 'b) t = 'a F.t

  let bimap f _ x = F.map f x
end)

module To_profunctor (F : Preface_specs.Contravariant.CORE) =
Profunctor.Via_dimap (struct
  type ('a, 'b) t = 'a F.t

  let dimap f _ x = F.contramap f x
end)
