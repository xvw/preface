module type LAWS = sig
  type ('a, 'b) t

  val semigroupoid_1 :
    unit -> (('a, 'b) t, ('c, 'a) t -> ('d, 'c) t -> ('d, 'b) t) Law.t
end

module For (S : Preface_specs.SEMIGROUPOID) :
  LAWS with type ('a, 'b) t := ('a, 'b) S.t = struct
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
