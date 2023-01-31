module type LAWS = sig
  type ('a, 'index) t

  val foldable_1 :
       unit
    -> ( (module Preface_specs.Types.T0 with type t = 'a)
       , ('b -> 'a -> 'a) -> 'a -> ('b, 'index) t -> 'a )
       Law.t

  val foldable_2 :
       unit
    -> ( (module Preface_specs.Types.T0 with type t = 'a)
       , ('a -> 'b -> 'a) -> 'a -> ('b, 'index) t -> 'a )
       Law.t

  val foldable_3 :
       unit
    -> ( (module Preface_specs.MONOID with type t = 'a)
       , ('a, 'index) t -> 'a )
       Law.t
end

module For (F : Preface_specs.INDEXED_FOLDABLE) :
  LAWS with type ('a, 'index) t := ('a, 'index) F.t = struct
  open Law

  let foldable_1 () =
    let lhs (type a) (module _ : Preface_specs.Types.T0 with type t = a) f z x =
      F.(fold_right f x z)
    and rhs (type a) (module T : Preface_specs.Types.T0 with type t = a) f z x =
      let module E = Util.Endo (T) in
      (F.fold_map (module E) f x) z
    in

    law ("fold_right f x z" =~ lhs) ("(fold_map (module Endo) f x) z" =~ rhs)
  ;;

  let foldable_2 () =
    let lhs (type a) (module _ : Preface_specs.Types.T0 with type t = a) f z x =
      F.(fold_left f z x)
    and rhs (type a) (module T : Preface_specs.Types.T0 with type t = a) f z x =
      let module E = Util.Endo (T) in
      let module D = Util.Dual (E) in
      let t = Fun.flip f in
      (F.fold_map (module D) t x) z
    in

    law ("fold_left f z x" =~ lhs)
      ("(fold_map (module Dual(Endo)) (Fun.flip f) x) z" =~ rhs)
  ;;

  let foldable_3 () =
    let lhs (type a) (module M : Preface_specs.MONOID with type t = a) x =
      F.reduce (module M) x
    and rhs (type a) (module M : Preface_specs.MONOID with type t = a) x =
      F.fold_map (module M) (fun x -> x) x
    in

    law ("reduce (module M)" =~ lhs) ("fold_map (module M) id" =~ rhs)
  ;;
end
