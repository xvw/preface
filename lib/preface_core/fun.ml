let compose_left_to_right f g x = g (f x)

let compose_right_to_left f g x = f (g x)

let constant x _ = x

module Infix = struct
  let ( %> ) = compose_left_to_right

  let ( <% ) = compose_right_to_left

  let ( % ) = compose_right_to_left
end

include Infix
include Stdlib.Fun
