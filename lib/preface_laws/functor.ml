module type LAWS = sig
  type 'a t

  include Indexed_functor.LAWS with type ('a, _) t := 'a t
end

module For (F : Preface_specs.FUNCTOR) : LAWS with type 'a t := 'a F.t = struct
  include Indexed_functor.For (Preface_make.Functor.Index (F))
end
