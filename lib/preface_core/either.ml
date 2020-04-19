type ('a, 'b) t =
  | Left of 'a
  | Right of 'b

let left x = Left x

let right x = Right x

let map_left f = function Left x -> Left (f x) | Right x -> Right x

let map_right f = function Left x -> Left x | Right x -> Right (f x)

let case e f g = (match e with Left a -> f a | Right b -> g b)
