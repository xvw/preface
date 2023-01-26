module type LAWS_APPLICATIVE = sig
  type 'a t

  val traversable_1 : unit -> ('a t, 'a t) Law.t

  module Compose (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) : sig
    module C : Preface_specs.APPLICATIVE with type 'a t = 'a G.t F.t

    val traversable_composition_1 :
         unit
      -> ( 'a -> 'b F.t
         , ('b -> 'c G.t) -> 'a t -> 'c t C.t )
         Law.t
  end

  module Naturality
      (F : Preface_specs.APPLICATIVE)
      (G : Preface_specs.APPLICATIVE) (NT : sig
        val run : 'a F.t -> 'a G.t
      end) : sig
    val traversable_naturality_1 :
      unit -> ('a -> 'b F.t, 'a t -> 'b t G.t) Law.t
  end
end

module type LAWS_MONAD = sig
  type 'a t

  val traversable_1 : unit -> ('a t, 'a t) Law.t
end

module For_monad (T : Preface_specs.Traversable.API_OVER_MONAD) :
  LAWS_MONAD with type 'a t := 'a T.t = struct
  open Law
  module TId = T.Traversable (Util.Id.Monad)

  let traversable_1 () =
    let lhs x = TId.traverse (fun x -> x) x
    and rhs x = x in

    law ("traverse id x" =~ lhs) ("x" =~ rhs)
  ;;
end

module For_applicative (T : Preface_specs.Traversable.API_OVER_APPLICATIVE) :
  LAWS_APPLICATIVE with type 'a t := 'a T.t = struct
  open Law
  module TId = T.Traversable (Util.Id.Applicative)

  let traversable_1 () =
    let lhs x = TId.traverse (fun x -> x) x
    and rhs x = x in

    law ("traverse id x" =~ lhs) ("x" =~ rhs)
  ;;

  module Compose (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) =
  struct
    open Preface_core.Fun.Infix
    module C = Preface_make.Applicative.Composition (F) (G)
    module TF = T.Traversable (F)
    module TG = T.Traversable (G)
    module TC = T.Traversable (C)

    let traversable_composition_1 () =
      let lhs f g x = F.map (TG.traverse g) (TF.traverse f x)
      and rhs f g x = TC.traverse (F.map g % f) x in

      law
        ("traverse (compose % map g % g)" =~ lhs)
        ("compose % map (traverse g) % traverse f" =~ rhs)
    ;;
  end

  module Naturality
      (F : Preface_specs.APPLICATIVE)
      (G : Preface_specs.APPLICATIVE) (NT : sig
        val run : 'a F.t -> 'a G.t
      end) =
  struct
    open Preface_core.Fun.Infix
    module TF = T.Traversable (F)
    module TG = T.Traversable (G)

    let traversable_naturality_1 () =
      let lhs f x = NT.run (TF.traverse f x)
      and rhs f x = TG.traverse (NT.run % f) x in

      law ("t % traverse f" =~ lhs) ("traverse (t % f)" =~ rhs)
    ;;
  end
end
