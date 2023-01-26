module type LAWS = sig
  type ('a, 'b) t

  val arrow_choice_1 :
       unit
    -> ('a -> 'b, (('a, 'c) Either.t, ('b, 'c) Either.t) t) Law.t

  val arrow_choice_2 :
       unit
    -> ( ('a, 'b) t
       ,    ('b, 'c) t
         -> (('a, 'd) Either.t, ('c, 'd) Either.t) t )
       Law.t

  val arrow_choice_3 :
       unit
    -> (('a, 'b) t, ('a, ('b, 'c) Either.t) t) Law.t

  val arrow_choice_4 :
       unit
    -> ( ('a, 'b) t
       , ('c -> 'd) -> (('a, 'c) Either.t, ('b, 'd) Either.t) t )
       Law.t

  val arrow_choice_5 :
       unit
    -> ( ('a, 'b) t
       , ( (('a, 'c) Either.t, 'd) Either.t
         , ('b, ('c, 'd) Either.t) Either.t )
         t )
       Law.t
end

module For (A : Preface_specs.ARROW_CHOICE) :
  LAWS with type ('a, 'b) t := ('a, 'b) A.t = struct
  open Law
  include Arrow.For (A)

  let arrow_choice_1 () =
    let lhs f = A.(left (arrow f))
    and rhs f = A.arrow Util.Fun.Arrow_choice.(left f) in

    law ("left (arrow f)" =~ lhs) ("arrow Fun.Arrow_choice.(left f)" =~ rhs)
  ;;

  let arrow_choice_2 () =
    let lhs f g = A.(left (f >>> g))
    and rhs f g = A.(left f >>> left g) in

    law ("left (f >>> g)" =~ lhs) ("left f >>> left g" =~ rhs)
  ;;

  let arrow_choice_3 () =
    let lhs f = A.(f >>> arrow Either.left)
    and rhs f = A.(arrow Either.left >>> left f) in

    law
      ("f >>> arrow Either.left" =~ lhs)
      ("arrow Either.left >>> left f" =~ rhs)
  ;;

  let arrow_choice_4 () =
    let lhs f g = A.(left f >>> arrow Util.Fun.Arrow_choice.(id +++ g))
    and rhs f g = A.(arrow Util.Fun.Arrow_choice.(id +++ g) >>> left f) in

    law
      ("left >>> arrow Fun.Arrow_choice.(id +++ g)" =~ lhs)
      ("arrow Fun.Arrow_choice.(id +++ g) >>> left f" =~ rhs)
  ;;

  let arrow_choice_5 () =
    let lhs f = A.(left (left f) >>> arrow Util.assoc_either)
    and rhs f = A.(arrow Util.assoc_either >>> left f) in

    law
      ("left (left f) >>> arrow assoc_either" =~ lhs)
      ("arrow assoc_either >>> left f" =~ rhs)
  ;;
end
