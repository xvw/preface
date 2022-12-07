module R (Req_m : Model.COVARIANT_1) (Req_t : Model.T0) = struct
  type 'a t = ('a * Req_t.t) Req_m.t

  let equal f a b =
    Req_m.equal
      (fun (x, tape_x) (y, tape_y) -> f x y && Req_t.equal tape_x tape_y)
      a b
  ;;

  let pp f ppf x =
    Format.fprintf ppf "%a"
      (Req_m.pp (fun ppf (x, tape) ->
           Format.fprintf ppf "(%a, %a)" f x Req_t.pp tape ) )
      x
  ;;

  let generator g = Req_m.generator (QCheck2.Gen.tup2 g Req_t.generator)

  let observable o =
    Req_m.observable (QCheck2.Observable.pair o Req_t.observable)
  ;;
end

module Suite
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.MONAD with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module W = Preface_make.Writer.Over_monad (M) (Tape)
  include Monad.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C) (D)
end

module Suite_functor
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.FUNCTOR with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module W = Preface_make.Writer.Functor (M) (Tape)
  include Functor.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C)
end

module Suite_invariant
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.FUNCTOR with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module F = Preface_make.Writer.Functor (M) (Tape)
  module W = Preface_make.Invariant.From_functor (F)
  include Invariant.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C)
end

module Suite_applicative
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.APPLICATIVE with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module W = Preface_make.Writer.Applicative (M) (Tape)
  include Applicative.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C)
end

module Suite_alternative
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.ALTERNATIVE with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0) =
struct
  module W = Preface_make.Writer.Alternative (M) (Tape)
  include Alternative.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C)
end

module Suite_monad_plus
    (Req_m : Model.COVARIANT_1)
    (M : Preface_specs.MONAD_PLUS with type 'a t = 'a Req_m.t)
    (Req_t : Model.T0)
    (Tape : Preface_specs.MONOID with type t = Req_t.t)
    (A : Model.T0)
    (B : Model.T0)
    (C : Model.T0)
    (D : Model.T0) =
struct
  module W = Preface_make.Writer.Monad_plus (M) (Tape)
  include Monad_plus.Suite (R (Req_m) (Req_t)) (W) (A) (B) (C) (D)
end
