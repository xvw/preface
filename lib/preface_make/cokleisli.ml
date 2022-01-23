open Preface_core.Fun.Infix

module Bifunctor (C : Preface_specs.Contravariant.CORE) =
Bifunctor.Via_bimap (struct
  type ('a, 'b) t = 'a C.t -> 'b

  let bimap f g p = C.contramap f %> p %> g
end)

module Profunctor (F : Preface_specs.Functor.CORE) =
Profunctor.Via_dimap (struct
  type ('a, 'b) t = 'a F.t -> 'b

  let dimap f g p = F.map f %> p %> g
end)

module Strong (C : Preface_specs.Comonad.CORE) =
Strong.Via_dimap_and_fst (struct
  type ('a, 'b) t = 'a C.t -> 'b

  let dimap f g p = C.map f %> p %> g
  let fst f x = (f (C.map Stdlib.fst x), Stdlib.snd (C.extract x))
end)

module Closed (F : Preface_specs.Functor.CORE) =
  Closed.Over_profunctor_via_closed
    (Profunctor
       (F))
       (struct
         type ('a, 'b) t = 'a F.t -> 'b

         let closed f g x = f (F.map (fun h -> h x) g)
       end)

module Category (C : Preface_specs.Comonad.CORE) =
Category.Via_id_and_compose (struct
  type ('a, 'b) t = 'a C.t -> 'b

  let id = C.extract
  let compose f g = C.compose_left_to_right g f
end)

module Arrow (C : Preface_specs.Comonad.CORE) =
  Arrow.From_strong_and_category (Strong (C)) (Category (C))
