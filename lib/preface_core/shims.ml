module Either = struct
  include Either

  let case f g = function Either.Left x -> f x | Either.Right x -> g x
end
