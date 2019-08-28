open Fun

module For (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t = struct
  include REQUIREMENT

  let replace value x = (map <% const) value x

  let void x = replace () x

  module Infix = struct
    let ( <$> ) = map

    let ( <&> ) x f = map f x

    let ( <$ ) value x = replace value x

    let ( $> ) x value = replace value x
  end

  include Infix
end
