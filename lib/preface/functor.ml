open Fun

module Via_map_with_replace (REQUIREMENT : Specs.Functor.FULL_REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t = struct
  include REQUIREMENT

  let void x = replace () x

  module Infix = struct
    let ( <$> ) = map

    let ( <&> ) x f = map f x

    let ( <$ ) value x = replace value x

    let ( $> ) x value = replace value x
  end

  include Infix
end

module Via_map (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t = struct
  module Requirement = struct
    include REQUIREMENT

    let replace value x = (map <% const) value x
  end

  include Via_map_with_replace (Requirement)
end
