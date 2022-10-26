let swap x =
  Stdlib.Either.fold ~left:Stdlib.Either.right ~right:Stdlib.Either.left x
;;
