type ('a, 'b) t = 'a -> 'b

include Preface_core.Fun

module Category = Preface_make.Category.Via_id_and_compose (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let id x = x

  let compose = compose_right_to_left
end)

module Arrow =
  Preface_make.Arrow.Over_category_and_via_arrow_an_split
    (Category)
    (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let arrow f = f

      let split f g (x, y) = (f x, g y)
    end)
