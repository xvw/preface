module type LAWS = sig
  module Arrow_zero : Preface_specs.ARROW_ZERO
  include Arrow.LAWS with module Arrow := Arrow_zero
end

module For (A : Preface_specs.ARROW_ZERO) : LAWS with module Arrow_zero := A =
  Arrow.For (A)
