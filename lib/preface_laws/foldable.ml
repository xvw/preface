module type LAWS = sig
  module Foldable : Preface_specs.FOLDABLE

  val foldable_fold_right_from_fold_map :
       unit
    -> ( (module Preface_specs.Types.T0 with type t = 'a)
       , ('b -> 'a -> 'a) -> 'a -> 'b Foldable.t -> 'a )
       Law.t

  val foldable_fold_left_from_fold_map :
       unit
    -> ( (module Preface_specs.Types.T0 with type t = 'a)
       , ('a -> 'b -> 'a) -> 'a -> 'b Foldable.t -> 'a )
       Law.t

  val foldable_reduce_is_fold_map_id :
       unit
    -> ( (module Preface_specs.MONOID with type t = 'a)
       , 'a Foldable.t -> 'a )
       Law.t
end

module For (F : Preface_specs.FOLDABLE) : LAWS with module Foldable := F =
struct
  open Law

  let foldable_fold_right_from_fold_map () =
    let lhs (type a) (module _ : Preface_specs.Types.T0 with type t = a) f z x =
      F.(fold_right f x z)
    and rhs (type a) (module T : Preface_specs.Types.T0 with type t = a) f z x =
      let module E = Util.Endo (T) in
      (F.fold_map (module E) f x) z
    in

    law "fold_right is fold_map using endo"
      ~lhs:("fold_right f x z" =~ lhs)
      ~rhs:("(fold_map (module Endo) f x) z" =~ rhs)
  ;;

  let foldable_fold_left_from_fold_map () =
    let lhs (type a) (module _ : Preface_specs.Types.T0 with type t = a) f z x =
      F.(fold_left f z x)
    and rhs (type a) (module T : Preface_specs.Types.T0 with type t = a) f z x =
      let module E = Util.Endo (T) in
      let module D = Util.Dual (E) in
      let t = Fun.flip f in
      (F.fold_map (module D) t x) z
    in

    law "fold_left is fold_map using dual + endo" ~lhs:("fold_left f z x" =~ lhs)
      ~rhs:("(fold_map (module Dual(Endo)) (Fun.flip f) x) z" =~ rhs)
  ;;

  let foldable_reduce_is_fold_map_id () =
    let lhs (type a) (module M : Preface_specs.MONOID with type t = a) x =
      F.reduce (module M) x
    and rhs (type a) (module M : Preface_specs.MONOID with type t = a) x =
      F.fold_map (module M) (fun x -> x) x
    in

    law "reduce must agree with fold_map id"
      ~lhs:("reduce (module M)" =~ lhs)
      ~rhs:("fold_map (module M) id" =~ rhs)
  ;;
end
