(** Building an {!module:Preface_specs.INDEXED_FOLDABLE} *)

(** {1 Using the minimal definition} *)

(** {2 Using fold_map}

    Build a {!module-type:Preface_specs.INDEXED_FOLDABLE} using
    {!module-type:Preface_specs.Indexed_foldable.WITH_FOLD_MAP}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_fold_map (Req : Preface_specs.Indexed_foldable.WITH_FOLD_MAP) :
  Preface_specs.INDEXED_FOLDABLE with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Using fold_right}

    Build a {!module-type:Preface_specs.INDEXED_FOLDABLE} using
    {!module-type:Preface_specs.Indexed_foldable.WITH_FOLD_RIGHT}.

    Standard method, using the minimal definition of an alt to derive its full
    API. *)

module Via_fold_right (Req : Preface_specs.Indexed_foldable.WITH_FOLD_RIGHT) :
  Preface_specs.INDEXED_FOLDABLE with type ('a, 'index) t = ('a, 'index) Req.t

(** {1 Manual construction}

    Advanced way to build a {!module-type:Preface_specs.INDEXED_FOLDABLE},
    constructing and assembling a component-by-component of
    {!module-type:Preface_specs.INDEXED_FOLDABLE}. (In order to provide your own
    implementation for some features.) *)

(** {2 Grouping of all components} *)

module Via
    (C : Preface_specs.Indexed_foldable.CORE)
    (O : Preface_specs.Indexed_foldable.OPERATION
           with type ('a, 'index) t = ('a, 'index) C.t) :
  Preface_specs.INDEXED_FOLDABLE with type ('a, 'index) t = ('a, 'index) O.t

(** {2 Building Core} *)

module Core_via_fold_right
    (Req : Preface_specs.Indexed_foldable.WITH_FOLD_RIGHT) :
  Preface_specs.Indexed_foldable.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

module Core_via_fold_map (Req : Preface_specs.Indexed_foldable.WITH_FOLD_MAP) :
  Preface_specs.Indexed_foldable.CORE
    with type ('a, 'index) t = ('a, 'index) Req.t

(** {2 Deriving Operation} *)

module Operation (C : Preface_specs.Indexed_foldable.CORE) :
  Preface_specs.Indexed_foldable.OPERATION
    with type ('a, 'index) t = ('a, 'index) C.t
