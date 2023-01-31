module type LAWS_MONOID = sig
  type 'a t

  include Indexed_monad_plus.LAWS_MONOID with type ('a, _) t := 'a t
end

module type LAWS_LEFT_ABSORPTION = sig
  type 'a t

  include Indexed_monad_plus.LAWS_LEFT_ABSORPTION with type ('a, _) t := 'a t
end

module type LAWS_LEFT_DISTRIBUTIVITY = sig
  type 'a t

  include
    Indexed_monad_plus.LAWS_LEFT_DISTRIBUTIVITY with type ('a, _) t := 'a t
end

module type LAWS_LEFT_CATCH = sig
  type 'a t

  include Indexed_monad_plus.LAWS_LEFT_CATCH with type ('a, _) t := 'a t
end

module For_monoidal (M : Preface_specs.MONAD_PLUS) :
  LAWS_MONOID with type 'a t := 'a M.t = struct
  include Indexed_monad_plus.For_monoidal (Preface_make.Monad_plus.Index (M))
end

module For_left_absorption (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_ABSORPTION with type 'a t := 'a M.t = struct
  include
    Indexed_monad_plus.For_left_absorption (Preface_make.Monad_plus.Index (M))
end

module For_left_distributivity (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_DISTRIBUTIVITY with type 'a t := 'a M.t = struct
  include
    Indexed_monad_plus.For_left_distributivity (Preface_make.Monad_plus.Index (M))
end

module For_left_catch (M : Preface_specs.MONAD_PLUS) :
  LAWS_LEFT_CATCH with type 'a t := 'a M.t = struct
  include Indexed_monad_plus.For_left_catch (Preface_make.Monad_plus.Index (M))
end
