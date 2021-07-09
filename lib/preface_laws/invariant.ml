module type LAWS = sig
  module Invariant : Preface_specs.INVARIANT

  val invariant_preserve_identity :
    unit -> ('a Invariant.t, 'a Invariant.t) Law.t

  val invariant_preserve_composition_of_morphism :
       unit
    -> ( 'a -> 'b
       ,    ('b -> 'a)
         -> ('c -> 'a)
         -> ('a -> 'c)
         -> 'c Invariant.t
         -> 'b Invariant.t )
       Law.t
end

module For (I : Preface_specs.INVARIANT) : LAWS with module Invariant := I =
struct
  open Law

  let invariant_preserve_identity () =
    let lhs x = I.invmap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law "Preserve identity morphisms" ~lhs:("invmap id id" =~ lhs)
      ~rhs:("id" =~ rhs)
  ;;

  let invariant_preserve_composition_of_morphism () =
    let open Preface_core.Fun in
    let lhs g g' f f' x = I.(invmap g g' % invmap f f') x
    and rhs g g' f f' x = I.invmap (g % f) (f' % g') x in

    law "Preserve composition of morphisms"
      ~lhs:("(invmap g g') % (invmap f f')" =~ lhs)
      ~rhs:("invmap (g % g') (f % f')" =~ rhs)
  ;;
end
