module type LIFT = sig
  type 'a t

  val lift_m : ('a -> 'b) -> 'a t -> 'b t

  val lift_m2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val lift_m3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val lift_a : ('a -> 'b) -> 'a t -> 'b t

  val lift_a2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val lift_a3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
end

module Lift_for_applicative_and_monad
    (A : Preface_specs.APPLICATIVE)
    (M : Preface_specs.MONAD with type 'a t = 'a A.t) =
struct
  let lift_m = M.lift

  let lift_m2 = M.lift2

  let lift_m3 = M.lift3

  let lift_a = A.lift

  let lift_a2 = A.lift2

  let lift_a3 = A.lift3
end

module From_applicative_and_monad
    (A : Preface_specs.APPLICATIVE)
    (M : Preface_specs.MONAD with type 'a t = 'a A.t) =
struct
  type 'a t = 'a M.t

  include (
    A :
      sig
        include Preface_specs.Applicative.CORE with type 'a t := 'a t

        include Preface_specs.Applicative.OPERATION with type 'a t := 'a t
      end )

  include (
    M :
      sig
        include Preface_specs.Monad.CORE with type 'a t := 'a t

        include Preface_specs.Monad.OPERATION with type 'a t := 'a t
      end )

  include (Lift_for_applicative_and_monad (A) (M) : LIFT with type 'a t := 'a t)

  module Infix = struct
    include A.Infix
    include M.Infix
  end

  module Syntax = struct
    include A.Syntax
    include M.Syntax
  end

  include Infix
  include Syntax
end

module From_alternative_and_monad_plus
    (A : Preface_specs.ALTERNATIVE)
    (M : Preface_specs.MONAD_PLUS with type 'a t = 'a A.t) =
struct
  type 'a t = 'a M.t

  include (
    A :
      sig
        include Preface_specs.Alternative.CORE with type 'a t := 'a t

        include Preface_specs.Alternative.OPERATION with type 'a t := 'a t
      end )

  include (
    M :
      sig
        include Preface_specs.Monad_plus.CORE with type 'a t := 'a t

        include Preface_specs.Monad_plus.OPERATION with type 'a t := 'a t
      end )

  include (Lift_for_applicative_and_monad (A) (M) : LIFT with type 'a t := 'a t)

  module Infix = struct
    include A.Infix
    include M.Infix
  end

  module Syntax = struct
    include A.Syntax
    include M.Syntax
  end

  include Infix
  include Syntax
end
