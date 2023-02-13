module type LAWS = sig
  type 'a t

  include Indexed_applicative.LAWS with type ('a, _) t := 'a t
end

module For (A : Preface_specs.APPLICATIVE) : LAWS with type 'a t := 'a A.t =
struct
  include Indexed_applicative.For (Preface_make.Applicative.Index (A))
end
