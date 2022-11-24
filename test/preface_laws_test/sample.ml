module Int = struct
  type t = int

  let generator = QCheck2.Gen.int
  let observable = QCheck2.Observable.int
  let pp ppf = Format.fprintf ppf "%d"
  let equal = Int.equal
end

module Float = struct
  type t = float

  let generator = QCheck2.Gen.float
  let observable = QCheck2.Observable.float
  let pp ppf = Format.fprintf ppf "%g"
  let equal = Float.equal
end

module String = struct
  type t = string

  let generator = QCheck2.Gen.string
  let observable = QCheck2.Observable.string
  let pp ppf = Format.fprintf ppf "%s"
  let equal = String.equal
end

module Char = struct
  type t = char

  let generator = QCheck2.Gen.char
  let observable = QCheck2.Observable.char
  let pp ppf = Format.fprintf ppf "%c"
  let equal = Char.equal
end

module Bool = struct
  type t = bool

  let generator = QCheck2.Gen.bool
  let observable = QCheck2.Observable.bool
  let pp ppf = Format.fprintf ppf "%b"
  let equal = Bool.equal
end
