module type LAWS_APPLICATIVE = sig
  module Applicative : Preface_specs.Traversable.API_OVER_APPLICATIVE

  val traversable_identity : unit -> ('a Applicative.t, 'a Applicative.t) Law.t

  module Compose (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) : sig
    module C : Preface_specs.APPLICATIVE with type 'a t = 'a G.t F.t

    val traversable_composition :
         unit
      -> ( 'a -> 'b F.t
         , ('b -> 'c G.t) -> 'a Applicative.t -> 'c Applicative.t C.t )
         Law.t
  end

  module Naturality
      (F : Preface_specs.APPLICATIVE)
      (G : Preface_specs.APPLICATIVE) (NT : sig
        val run : 'a F.t -> 'a G.t
      end) : sig
    val traversable_naturality :
      unit -> ('a -> 'b F.t, 'a Applicative.t -> 'b Applicative.t G.t) Law.t
  end
end

module type LAWS_MONAD = sig
  module Monad : Preface_specs.Traversable.API_OVER_MONAD

  val traversable_identity : unit -> ('a Monad.t, 'a Monad.t) Law.t
end

module For_monad (T : Preface_specs.Traversable.API_OVER_MONAD) :
  LAWS_MONAD with module Monad := T = struct
  open Law
  module TId = T.Traversable (Util.Id.Monad)

  let traversable_identity () =
    let lhs x = TId.traverse (fun x -> x) x
    and rhs x = x in

    law "naturality" ~lhs:("traverse id x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;
end

module For_applicative (T : Preface_specs.Traversable.API_OVER_APPLICATIVE) :
  LAWS_APPLICATIVE with module Applicative := T = struct
  open Law
  module TId = T.Traversable (Util.Id.Applicative)

  let traversable_identity () =
    let lhs x = TId.traverse (fun x -> x) x
    and rhs x = x in

    law "naturality" ~lhs:("traverse id x" =~ lhs) ~rhs:("x" =~ rhs)
  ;;

  module Compose (F : Preface_specs.APPLICATIVE) (G : Preface_specs.APPLICATIVE) =
  struct
    open Preface_core.Fun.Infix
    module C = Preface_make.Applicative.Composition (F) (G)
    module TF = T.Traversable (F)
    module TG = T.Traversable (G)
    module TC = T.Traversable (C)

    let traversable_composition () =
      let lhs f g x = F.map (TG.traverse g) (TF.traverse f x)
      and rhs f g x = TC.traverse (F.map g % f) x in

      law "composition"
        ~lhs:("traverse (compose % map g % g)" =~ lhs)
        ~rhs:("compose % map (traverse g) % traverse f" =~ rhs)
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

    let traversable_naturality () =
      let lhs f x = NT.run (TF.traverse f x)
      and rhs f x = TG.traverse (NT.run % f) x in
      law "naturality" ~lhs:("t % traverse f" =~ lhs)
        ~rhs:("traverse (t % f)" =~ rhs)
    ;;
  end
end
