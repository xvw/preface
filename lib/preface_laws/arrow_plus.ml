module type LAWS = sig
  module Arrow_plus : Preface_specs.ARROW_PLUS
  include Arrow.LAWS with module Arrow := Arrow_plus
end

module For (A : Preface_specs.ARROW_PLUS) : LAWS with module Arrow_plus := A =
  Arrow.For (A)
