module Core_via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t = struct
  include F

  let fold_map' neutral combine f x =
    let reducer x acc = combine (f x) acc in
    fold_right reducer x neutral
  ;;
end

module Core_via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t = struct
  include F

  let fold_right f x acc =
    (fold_map' (fun x -> x) (fun f g x -> f (g x)) f) x acc
  ;;
end

module Operation (C : Preface_specs.Foldable.CORE) :
  Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t = struct
  type 'a t = 'a C.t

  let fold_map (type m) (module M : Preface_specs.Monoid.CORE with type t = m) f
      x =
    C.fold_map' M.neutral M.combine f x
  ;;

  let reduce (type m) (module M : Preface_specs.Monoid.CORE with type t = m) x =
    fold_map (module M) (fun x -> x) x
  ;;

  let fold_left f acc x =
    let f' x k z = k (f z x) in
    let fold = C.fold_right f' x (fun x -> x) in
    fold acc
  ;;

  let for_all p x = C.fold_map' true ( && ) p x

  let exists p x = C.fold_map' false ( || ) p x

  let length x = fold_left (fun acc _ -> succ acc) 0 x
end

module Via
    (C : Preface_specs.Foldable.CORE)
    (O : Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t) :
  Preface_specs.FOLDABLE with type 'a t = 'a O.t = struct
  include C

  include (O : Preface_specs.Foldable.OPERATION with type 'a t := 'a t)
end

module Via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t = struct
  module C = Core_via_fold_right (F)
  module O = Operation (C)
  include Via (C) (O)
end

module Via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t = struct
  module C = Core_via_fold_map (F)
  module O = Operation (C)
  include Via (C) (O)
end
