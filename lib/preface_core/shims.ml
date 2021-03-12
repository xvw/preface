module Either = struct
  include Either

  let case f g = function Either.Left x -> f x | Either.Right x -> g x

  let swap x = Either.fold ~left:Either.right ~right:Either.left x
end
