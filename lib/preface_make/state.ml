module Core_over_monad
    (M : Preface_specs.MONAD)
    (State : Preface_specs.Types.T0) =
struct
  type state = State.t

  type 'a monad = 'a M.t

  type 'a t = state -> ('a * state) monad

  let state f x = M.return (f x)

  let eval m s = M.map fst (m s)

  let exec m s = M.map snd (m s)

  let run m s = m s

  let get s = state (fun s -> (s, s)) s

  let set s = state (fun _ -> ((), s))

  let modify f = state (fun s -> ((), f s))

  let gets f = state (fun s -> (f s, s))
end

module Functor (F : Preface_specs.FUNCTOR) (State : Preface_specs.Types.T0) =
Functor.Via_map (struct
  type 'a t = State.t -> ('a * State.t) F.t

  let map f m s = F.map (fun (x, s) -> (f x, s)) (m s)
end)

module Applicative (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) =
Applicative.Via_apply (struct
  type 'a t = State.t -> ('a * State.t) M.t

  let pure x s = M.return (x, s)

  let apply mf mx s =
    let open M in
    let* (f, x) = mf s in
    let+ (y, st) = mx x in
    (f y, st)
  ;;
end)

module Monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) =
Monad.Via_bind (struct
  type 'a t = State.t -> ('a * State.t) M.t

  let return x s = M.return (x, s)

  let bind f m s =
    let open M in
    let* (v, st) = m s in
    f v st
  ;;
end)

module Monad_plus
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) =
  Monad_plus.Over_monad
    (Monad (M) (State))
       (struct
         type 'a t = State.t -> ('a * State.t) M.t

         let neutral _ = M.neutral

         let combine state_l state_r s = M.combine (state_l s) (state_r s)
       end)

module Alternative
    (M : Preface_specs.MONAD_PLUS)
    (State : Preface_specs.Types.T0) =
  Alternative.Over_applicative
    (Applicative (M) (State))
       (struct
         type 'a t = State.t -> ('a * State.t) M.t

         let neutral _ = M.neutral

         let combine state_l state_r s = M.combine (state_l s) (state_r s)
       end)

module Over_monad (M : Preface_specs.MONAD) (State : Preface_specs.Types.T0) =
struct
  include Core_over_monad (M) (State)
  module Monad = Monad (M) (State)
  include Monad
end
