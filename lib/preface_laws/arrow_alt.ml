module type LAWS = sig
  module Arrow_alt : Preface_specs.ARROW_ALT
  include Arrow.LAWS with module Arrow := Arrow_alt
end

module For (A : Preface_specs.ARROW_ALT) : LAWS with module Arrow_alt := A =
  Arrow.For (A)
