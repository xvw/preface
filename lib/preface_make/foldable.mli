module Via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t

module Via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.FOLDABLE with type 'a t = 'a F.t

module Via
    (C : Preface_specs.Foldable.CORE)
    (O : Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t) :
  Preface_specs.FOLDABLE with type 'a t = 'a O.t

module Core_via_fold_right (F : Preface_specs.Foldable.CORE_WITH_FOLD_RIGHT) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t

module Core_via_fold_map (F : Preface_specs.Foldable.CORE_WITH_FOLD_MAP) :
  Preface_specs.Foldable.CORE with type 'a t = 'a F.t

module Operation (C : Preface_specs.Foldable.CORE) :
  Preface_specs.Foldable.OPERATION with type 'a t = 'a C.t
