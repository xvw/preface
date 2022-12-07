module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_ZERO with type ('a, 'b) t = ('a, 'b) R.t) =
  Arrow.Suite (R) (P)
