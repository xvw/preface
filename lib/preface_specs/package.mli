(** Describes the conjunction of multiple implementation> Ie: Applicative and
    Monad.*)

(** Lift Operations for Applicative and Monad *)
module type LIFT = sig
  type 'a t

  val lift_m : ('a -> 'b) -> 'a t -> 'b t
  (** Monadic version of [lift]. *)

  val lift_m2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Monadic version of [lift2]. *)

  val lift_m3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Monadic version of [lift3]. *)

  val lift_a : ('a -> 'b) -> 'a t -> 'b t
  (** Applicative version of [lift]. *)

  val lift_a2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Applicative version of [lift2]. *)

  val lift_a3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** Applicative version of [lift3]. *)
end

(** {2 Package of an Applicative and a Monad} *)

module type APPLICATIVE_AND_MONAD = sig
  type 'a t

  include Applicative.CORE with type 'a t := 'a t

  include Applicative.OPERATION with type 'a t := 'a t

  include Monad.CORE with type 'a t := 'a t

  include Monad.OPERATION with type 'a t := 'a t

  include LIFT with type 'a t := 'a t

  module Infix : sig
    include Applicative.INFIX with type 'a t := 'a t

    include Monad.INFIX with type 'a t := 'a t
  end

  module Syntax : sig
    include Applicative.SYNTAX with type 'a t := 'a t

    include Monad.SYNTAX with type 'a t := 'a t
  end

  include module type of Infix

  include module type of Syntax
end

(** {2 Package of an Alternative and a Monad_plus} *)

module type ALTERNATIVE_AND_MONAD_PLUS = sig
  type 'a t

  include Alternative.CORE with type 'a t := 'a t

  include Alternative.OPERATION with type 'a t := 'a t

  include Monad_plus.CORE with type 'a t := 'a t

  include Monad_plus.OPERATION with type 'a t := 'a t

  include LIFT with type 'a t := 'a t

  module Infix : sig
    include Alternative.INFIX with type 'a t := 'a t

    include Monad_plus.INFIX with type 'a t := 'a t
  end

  module Syntax : sig
    include Alternative.SYNTAX with type 'a t := 'a t

    include Monad_plus.SYNTAX with type 'a t := 'a t
  end

  include module type of Infix

  include module type of Syntax
end
