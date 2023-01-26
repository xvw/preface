module type LAWS = sig
  type 'a t

  val invariant_1 : unit -> ('a t, 'a t) Law.t

  val invariant_2 :
       unit
    -> ( 'a -> 'b
       ,    ('b -> 'a)
         -> ('c -> 'a)
         -> ('a -> 'c)
         -> 'c t
         -> 'b t )
       Law.t
end

module For (I : Preface_specs.INVARIANT) : LAWS with type 'a t := 'a I.t =
struct
  open Law

  let invariant_1 () =
    let lhs x = I.invmap (fun x -> x) (fun x -> x) x
    and rhs x = x in

    law ("invmap id id" =~ lhs) ("id" =~ rhs)
  ;;

  let invariant_2 () =
    let open Preface_core.Fun in
    let lhs g g' f f' x = I.(invmap g g' % invmap f f') x
    and rhs g g' f f' x = I.invmap (g % f) (f' % g') x in

    law
      ("(invmap g g') % (invmap f f')" =~ lhs)
      ("invmap (g % g') (f % f')" =~ rhs)
  ;;
end
