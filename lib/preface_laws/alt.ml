module type LAWS = sig
  type 'a t

  include Indexed_alt.LAWS with type ('a, _) t := 'a t
end

module For (A : Preface_specs.ALT) : LAWS with type 'a t := 'a A.t = struct
  include Indexed_alt.For (Preface_make.Alt.Index (A))
end
