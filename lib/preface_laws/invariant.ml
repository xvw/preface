module type LAWS = sig
  module Invariant : Preface_specs.INVARIANT

  val invariant_1 : unit -> ('a Invariant.t, 'a Invariant.t) Law.t

  val invariant_2 :
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

  let invariant_1 () =
    let lhs x = I.invmap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law ~lhs:("invmap id id" =~ lhs) ~rhs:("id" =~ rhs)
  ;;

  let invariant_2 () =
    let open Preface_core.Fun in
    let lhs g g' f f' x = I.(invmap g g' % invmap f f') x
    and rhs g g' f f' x = I.invmap (g % f) (f' % g') x in

    law
      ~lhs:("(invmap g g') % (invmap f f')" =~ lhs)
      ~rhs:("invmap (g % g') (f % f')" =~ rhs)
  ;;
end
