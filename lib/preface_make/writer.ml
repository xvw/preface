module Core_over_monad
    (Monad : Preface_specs.MONAD)
    (Tape : Preface_specs.MONOID) =
struct
  type tape = Tape.t
  type 'a monad = 'a Monad.t
  type 'a t = ('a * tape) monad

  let upper m = Monad.bind (fun a -> Monad.return (a, Tape.neutral)) m
  let writer (x, t) = Monad.return (x, t)
  let run writer_m = writer_m
  let exec x = Monad.map snd x
  let tell s = writer ((), s)
  let listen m = Monad.map (fun (x, b) -> ((x, b), b)) m
  let listens f m = Monad.map (fun (x, b) -> ((x, f b), b)) m
  let pass m = Monad.map (fun ((x, f), b) -> (x, f b)) m
  let censor f m = Monad.map (fun (x, b) -> (x, f b)) m
end

module Functor (F : Preface_specs.FUNCTOR) (Tape : Preface_specs.MONOID) =
Functor.Via_map (struct
  type 'a t = ('a * Tape.t) F.t

  let map f x = F.map (fun (y, t) -> (f y, t)) x
end)

module Applicative (A : Preface_specs.APPLICATIVE) (Tape : Preface_specs.MONOID) =
Applicative.Via_apply (struct
  type 'a t = ('a * Tape.t) A.t

  let pure x = A.pure (x, Tape.neutral)

  let apply f v =
    let g (x, t) (y, u) = (x y, Tape.combine t u) in
    A.lift2 g f v
  ;;
end)

module Alternative (A : Preface_specs.ALTERNATIVE) (Tape : Preface_specs.MONOID) =
  Alternative.Over_applicative
    (Applicative (A) (Tape))
       (struct
         type 'a t = ('a * Tape.t) A.t

         let neutral = A.neutral
         let combine writer_l writer_r = A.combine writer_l writer_r
       end)

module Monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) =
Monad.Via_bind (struct
  type 'a t = ('a * Tape.t) M.t

  let return x = M.return (x, Tape.neutral)

  let bind f m =
    M.(m >>= fun (x, t) -> f x >|= fun (y, u) -> (y, Tape.combine t u))
  ;;
end)

module Monad_plus (M : Preface_specs.MONAD_PLUS) (Tape : Preface_specs.MONOID) =
  Monad_plus.Over_monad
    (Monad (M) (Tape))
       (struct
         type 'a t = ('a * Tape.t) M.t

         let neutral = M.neutral
         let combine writer_l writer_r = M.combine writer_l writer_r
       end)

module Over_monad (M : Preface_specs.MONAD) (Tape : Preface_specs.MONOID) =
struct
  include Core_over_monad (M) (Tape)
  module Monad = Monad (M) (Tape)
  include Monad
end
