module type LAWS = sig
  type 'a t

  val alt_1 : unit -> ('a t, 'a t -> 'a t -> 'a t) Law.t
  val alt_2 : unit -> ('a -> 'b, 'a t -> 'a t -> 'b t) Law.t
end

module For (A : Preface_specs.ALT) : LAWS with type 'a t := 'a A.t = struct
  open Law
  include Functor.For (A)

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
