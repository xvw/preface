type t = |

let absurd : t -> 'a = function _ -> .

let left x = Either.fold ~left:Fun.id ~right:absurd x

let right x = Either.fold ~left:absurd ~right:Fun.id x
