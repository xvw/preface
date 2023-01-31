module type LAWS = sig
  type 'a t

  include Indexed_foldable.LAWS with type ('a, _) t := 'a t
end

module For (F : Preface_specs.FOLDABLE) : LAWS with type 'a t := 'a F.t = struct
  include Indexed_foldable.For (Preface_make.Foldable.Index (F))
end
