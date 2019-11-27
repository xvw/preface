module Make_free_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FUNCTOR with type 'a t = 'a Preface_specs.Free.CORE(F).t =
Functor.Make_via_map(struct
  include Preface_specs.Free.CORE (F)

  let rec map f = function
    | Return v -> Return (f v)
    | Bind f' -> Bind (F.map (map f) f')
end)

module Make_free_applicative (F : Preface_specs.Functor.CORE) :
  Preface_specs.APPLICATIVE with type 'a t = 'a Preface_specs.Free.CORE(F).t =
Applicative.Make_via_apply (struct
  include Preface_specs.Free.CORE (F)
  include Make_free_functor (F)

  let pure a = Return a

  let rec apply f a =
    match f with
    | Return f' -> map f' a
    | Bind f' -> Bind (F.map (fun f -> apply f a) f')
end)

module Make_free_monad (F : Preface_specs.Functor.CORE) :
  Preface_specs.MONAD with type 'a t = 'a Preface_specs.Free.CORE(F).t =
Monad.Make_via_bind (struct
  include Preface_specs.Free.CORE (F)
  include Make_free_functor (F)
  include Make_free_applicative (F)

  let return = pure

  let rec bind f = function
    | Return a -> f a
    | Bind a -> Bind (F.map (bind f) a)
end)
