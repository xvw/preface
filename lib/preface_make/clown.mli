(** [Clown] can produces [Bifunctor] or [Profunctor] using a [Functor] (or a
    [Contravariant]) on the first argument of the [Bi/Profunctor] as described
    in
    {{:https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.475.6134&rep=rep1&type=pdf}
    Clowns to the Left, Jokers to the Right (Functional Pearl)}*)

(** {2 Bifunctor}

    Produces a {!module-type:Preface_specs.BIFUNCTOR} using a
    {!module-type:Preface_specs.FUNCTOR} as first argument.*)

module To_bifunctor (F : Preface_specs.Functor.CORE) :
  Preface_specs.BIFUNCTOR with type ('a, 'b) t = 'a F.t

(** {2 Profunctor}

    Produces a {!module-type:Preface_specs.PROFUNCTOR} using a
    {!module-type:Preface_specs.CONTRAVARIANT} as first argument.*)

module To_profunctor (F : Preface_specs.Contravariant.CORE) :
  Preface_specs.PROFUNCTOR with type ('a, 'b) t = 'a F.t
