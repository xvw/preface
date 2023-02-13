module type LAWS = sig
  type 'a t

  include Indexed_monad.LAWS with type ('a, _) t := 'a t
end

module For (M : Preface_specs.MONAD) : LAWS with type 'a t := 'a M.t = struct
  include Indexed_monad.For (Preface_make.Monad.Index (M))
end
