(** Modules for building [Free_selective] modules. *)

(** {2 Building a free rigid selective over a functor With [Either] from
    [Preface_core]}

    Given a functor, it produce a selective module with a natural transformation
    in order to extract the underlying value. *)

module Over_functor (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_SELECTIVE
    with type 'a f = 'a F.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

(** {2 Building a free applicative over an applicative With [Either] from
    [Preface_core]}

    Given an applicative, it produce a selective module with a natural
    transformation in order to extract the underlying value. *)

module Over_applicative (F : Preface_specs.Applicative.CORE) :
  Preface_specs.FREE_SELECTIVE
    with type 'a f = 'a F.t
     and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

(** {2 Building a free selective over a selective With [Either] from
    [Preface_core]}

    Given a selective, it produce a selective module with a natural
    transformation in order to extract the underlying value and a [run] function
    wich is the natural transformation from the underlying selective using the
    identity. *)

(** {1 Building modules with Parametrized [Either].} *)

module Over_selective
    (F : Preface_specs.Selective.CORE
           with type ('a, 'b) either = ('a, 'b) Preface_core.Either.t) : sig
  include
    Preface_specs.FREE_SELECTIVE
      with type 'a f = 'a F.t
       and type ('a, 'b) either = ('a, 'b) Preface_core.Either.t

  val run : 'a t -> 'a f
end

module Over_functor_and_either
    (Either : Preface_core.Requirements.EITHER)
    (F : Preface_specs.Functor.CORE) :
  Preface_specs.FREE_SELECTIVE
    with type 'a f = 'a F.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Over_applicative_and_either
    (Either : Preface_core.Requirements.EITHER)
    (F : Preface_specs.Applicative.CORE) :
  Preface_specs.FREE_SELECTIVE
    with type 'a f = 'a F.t
     and type ('a, 'b) either = ('a, 'b) Either.t

module Over_selective_and_either
    (Either : Preface_core.Requirements.EITHER)
    (F : Preface_specs.Selective.CORE
           with type ('a, 'b) either = ('a, 'b) Either.t) : sig
  include
    Preface_specs.FREE_SELECTIVE
      with type 'a f = 'a F.t
       and type ('a, 'b) either = ('a, 'b) Either.t

  val run : 'a t -> 'a f
end
