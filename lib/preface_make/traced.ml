module Core_over_comonad
    (C : Preface_specs.COMONAD)
    (Tape : Preface_specs.MONOID) =
struct
  type tape = Tape.t
  type 'a comonad = 'a C.t
  type 'a t = (tape -> 'a) comonad

  let run f = f
  let lower f = (C.map (fun g -> g Tape.neutral)) f
  let trace t f = (C.extract f) t
  let traces f g = trace (f ((C.extract g) Tape.neutral)) g
  let listen t = run (C.map (fun f m -> (f m, m))) t
  let listens g t = run (C.map (fun f m -> (f m, g m))) t
  let censor g t = run (C.map (fun f x -> f (g x))) t
end

module Functor (F : Preface_specs.FUNCTOR) (Tape : Preface_specs.MONOID) =
Functor.Via_map (struct
  type 'a t = (Tape.t -> 'a) F.t

  let map f w = F.map (fun g t -> f (g t)) w
end)

module Comonad (C : Preface_specs.COMONAD) (Tape : Preface_specs.MONOID) =
Comonad.Via_extend (struct
  type 'a t = (Tape.t -> 'a) C.t

  let extract g = C.extract g Tape.neutral

  let extend f w =
    let open C in
    (fun wf m -> f (map (fun g n -> g (Tape.combine m n)) wf)) <<= w
  ;;
end)

module Over_comonad (C : Preface_specs.COMONAD) (Tape : Preface_specs.MONOID) =
struct
  include Core_over_comonad (C) (Tape)
  module Comonad = Comonad (C) (Tape)
  include Comonad
end
