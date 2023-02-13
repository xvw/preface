module type LAWS = sig
  type 'a t

  include Indexed_bind.LAWS with type ('a, _) t := 'a t
end

module For (B : Preface_specs.BIND) : LAWS with type 'a t := 'a B.t = struct
  include Indexed_bind.For (Preface_make.Bind.Index (B))
end
