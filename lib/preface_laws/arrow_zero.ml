module type LAWS = sig
  type ('a, 'b) t
end

module For (A : Preface_specs.ARROW_ZERO) : LAWS with type ('a, 'b) t := ('a, 'b) A.t =
  Arrow.For (A)
