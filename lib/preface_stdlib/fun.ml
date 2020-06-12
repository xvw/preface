type ('a, 'b) t = 'a -> 'b

include Preface_core.Fun

module Category = Preface_make.Category.Via_id_and_compose (struct
  type nonrec ('a, 'b) t = ('a, 'b) t

  let id x = x

  let compose = compose_right_to_left
end)
