module Int = struct
  type t = int

  let arbitrary = QCheck.int
  let observable = QCheck.Observable.int
  let equal = Int.equal
end

module String = struct
  type t = string

  let arbitrary = QCheck.small_printable_string
  let observable = QCheck.Observable.string
  let equal = String.equal
end

module Float = struct
  type t = float

  let arbitrary = QCheck.float
  let observable = QCheck.Observable.float
  let equal = Float.equal
end

module type PACKAGE = sig
  module A : Model.T0
  module B : Model.T0
  module C : Model.T0
  module D : Model.T0
  module E : Model.T0
  module F : Model.T0
  module G : Model.T0
end

module Pack1 = struct
  module A = Int
  module B = String
  module C = Float
  module D = String
  module E = Float
  module F = Int
  module G = String
end
