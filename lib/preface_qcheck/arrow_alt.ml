module Suite
    (R : Model.PROFUNCTORIAL)
    (P : Preface_specs.ARROW_ALT with type ('a, 'b) t = ('a, 'b) R.t) =
  Arrow.Suite (R) (P)
