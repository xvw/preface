module type LAWS = sig
  type 'a t

  include Indexed_selective.LAWS with type ('a, _) t := 'a t
end

module type LAWS_RIGID = sig
  type 'a t

  include Indexed_selective.LAWS_RIGID with type ('a, _) t := 'a t
end

module For (S : Preface_specs.SELECTIVE) : LAWS with type 'a t := 'a S.t =
struct
  include Indexed_selective.For (Preface_make.Selective.Index (S))
end

module For_rigid (S : Preface_specs.SELECTIVE) :
  LAWS_RIGID with type 'a t := 'a S.t = struct
  include Indexed_selective.For_rigid (Preface_make.Selective.Index (S))
end
