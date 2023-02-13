module type LAWS_MONOID = sig
  type 'a t

  include Indexed_alternative.LAWS_MONOID with type ('a, _) t := 'a t
end

module type LAWS_RIGHT_DISTRIBUTIVITY = sig
  type 'a t

  include
    Indexed_alternative.LAWS_RIGHT_DISTRIBUTIVITY with type ('a, _) t := 'a t
end

module type LAWS_RIGHT_ABSORPTION = sig
  type 'a t

  include Indexed_alternative.LAWS_RIGHT_ABSORPTION with type ('a, _) t := 'a t
end

module For_monoidal (A : Preface_specs.ALTERNATIVE) :
  LAWS_MONOID with type 'a t := 'a A.t = struct
  include Indexed_alternative.For_monoidal (Preface_make.Alternative.Index (A))
end

module For_right_distributivity (A : Preface_specs.ALTERNATIVE) :
  LAWS_RIGHT_DISTRIBUTIVITY with type 'a t := 'a A.t = struct
  include
    Indexed_alternative.For_right_distributivity
      (Preface_make.Alternative.Index (A))
end

module For_right_absorbtion (A : Preface_specs.ALTERNATIVE) :
  LAWS_RIGHT_ABSORPTION with type 'a t := 'a A.t = struct
  include
    Indexed_alternative.For_right_absorbtion (Preface_make.Alternative.Index (A))
end
