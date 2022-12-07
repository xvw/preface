module type LAWS = sig
  module Semigroupoid : Preface_specs.SEMIGROUPOID

  val semigroupoid_1 :
       unit
    -> ( ('a, 'b) Semigroupoid.t
       ,    ('c, 'a) Semigroupoid.t
         -> ('d, 'c) Semigroupoid.t
         -> ('d, 'b) Semigroupoid.t )
       Law.t
end

module For (S : Preface_specs.SEMIGROUPOID) :
  LAWS with module Semigroupoid := S = struct
  open Law

  let semigroupoid_1 () =
    let lhs f g h =
      let r = S.(g % h) in
      S.(f % r)
    and rhs f g h =
      let r = S.(f % g) in
      S.(r % h)
    in

    law ("f % (g % h)" =~ lhs) ("(f % g) % h" =~ rhs)
  ;;
end
