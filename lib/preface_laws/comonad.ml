module type LAWS = sig
  type 'a t

  include Indexed_comonad.LAWS with type ('a, _) t := 'a t
end

module For (C : Preface_specs.COMONAD) : LAWS with type 'a t := 'a C.t = struct
  include Indexed_comonad.For (Preface_make.Comonad.Index (C))
end
