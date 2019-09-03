open Fun

module Make_with_replace (REQUIREMENT : Specs.Functor.REQUIREMENT_WITH_REPLACE) :
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

module Make (REQUIREMENT : Specs.Functor.REQUIREMENT) :
  Specs.FUNCTOR with type 'a t = 'a REQUIREMENT.t = struct
  module Requirement = struct
    include REQUIREMENT

    let replace value x = (map <% const) value x
  end

  include Make_with_replace (Requirement)
end
