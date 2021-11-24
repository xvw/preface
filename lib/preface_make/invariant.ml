module Via_invmap (I : Preface_specs.Invariant.WITH_INVMAP) = I

module From_functor (F : Preface_specs.Functor.CORE) = Via_invmap (struct
  type 'a t = 'a F.t

  let invmap f _ = F.map f
end)

module From_contravariant (F : Preface_specs.Contravariant.CORE) =
Via_invmap (struct
  type 'a t = 'a F.t

  let invmap _ f = F.contramap f
end)
