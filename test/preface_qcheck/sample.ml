module type GENERATOR = sig
  type input

  val arbitrary : input Arbitrary.t

  val observable : input QCheck.Observable.t

  val eq : input -> input -> bool
end

module type PACK = sig
  module T1 : GENERATOR

  module T2 : GENERATOR

  module T3 : GENERATOR

  module T4 : GENERATOR

  module T5 : GENERATOR

  module T6 : GENERATOR
end

module Int = struct
  type input = int

  let arbitrary = QCheck.int

  let observable = QCheck.Observable.int

  let eq = Int.equal
end

module String = struct
  type input = string

  let arbitrary = QCheck.small_printable_string

  let observable = QCheck.Observable.string

  let eq = String.equal
end

module Float = struct
  type input = float

  let arbitrary = QCheck.float

  let observable = QCheck.Observable.float

  let eq = Float.equal
end

module Pack = struct
  module T1 = Int
  module T2 = String
  module T3 = Float
  module T4 = Int
  module T5 = String
  module T6 = Float
end
