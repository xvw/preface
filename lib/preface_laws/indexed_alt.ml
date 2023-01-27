module type LAWS = sig
  type ('a, 'index) t

  val alt_1 :
       unit
    -> ( ('a, 'index) t
       , ('a, 'index) t -> ('a, 'index) t -> ('a, 'index) t )
       Law.t

  val alt_2 :
    unit -> ('a -> 'b, ('a, 'index) t -> ('a, 'index) t -> ('b, 'index) t) Law.t
end

module For (A : Preface_specs.INDEXED_ALT) :
  LAWS with type ('a, 'index) t := ('a, 'index) A.t = struct
  open Law
  include Indexed_functor.For (A)

  let alt_1 () =
    let lhs a b c = A.(Infix.(a <|> b) <|> c)
    and rhs a b c = A.(a <|> Infix.(b <|> c)) in

    law ("(a <|> b) <|> c" =~ lhs) ("a <|> (b <|> c)" =~ rhs)
  ;;

  let alt_2 () =
    let lhs f a b = A.(f <$> Infix.(a <|> b))
    and rhs f a b = A.(Infix.(f <$> a) <|> Infix.(f <$> b)) in

    law ("f <$> (a <|> b)" =~ lhs) ("(f <$> a) <|> (f <$> b)" =~ rhs)
  ;;
end
