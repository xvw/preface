module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_PLUS with type ('a, 'b) t = ('a, 'b) R.t) =
  Arrow.Suite (R) (P)
