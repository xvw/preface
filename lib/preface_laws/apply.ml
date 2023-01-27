module type LAWS = sig
  type 'a t

  include Indexed_apply.LAWS with type ('a, _) t := 'a t
end

module For (A : Preface_specs.APPLY) : LAWS with type 'a t := 'a A.t = struct
  include Indexed_apply.For (Preface_make.Apply.Index (A))
end
