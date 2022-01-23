let compose_left_to_right f g x = g (f x)
let compose_right_to_left f g x = f (g x)
let const x _ = x
let id x = x
let flip f x y = f y x

module Infix = struct
  let ( %> ) = compose_left_to_right
  let ( <% ) = compose_right_to_left
  let ( % ) = compose_right_to_left
end

include Infix
