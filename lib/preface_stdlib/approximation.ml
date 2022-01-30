module Over (M : Preface_specs.MONOID) = struct
  type _ t = Over of M.t

  module Applicative = Preface_make.Applicative.Via_pure_and_apply (struct
    type nonrec 'a t = 'a t

    let pure _ = Over M.neutral
    let apply (Over x) (Over y) = Over M.(x <|> y)
  end)

  module Selective =
    Preface_make.Selective.Over_applicative_via_select
      (Applicative)
      (struct
        type nonrec 'a t = 'a t

        let select (Over x) (Over y) = Over M.(x <|> y)
      end)

  let get (Over x) = x
end

module Under (M : Preface_specs.MONOID) = struct
  type _ t = Under of M.t

  module Applicative = Preface_make.Applicative.Via_pure_and_apply (struct
    type nonrec 'a t = 'a t

    let pure _ = Under M.neutral
    let apply (Under x) (Under y) = Under M.(x <|> y)
  end)

  module Selective =
    Preface_make.Selective.Over_applicative_via_select
      (Applicative)
      (struct
        type nonrec 'a t = 'a t

        let select (Under x) _ = Under x
      end)

  let get (Under x) = x
end
