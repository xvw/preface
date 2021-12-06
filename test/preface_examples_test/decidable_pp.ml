(* Example coming from https://www.youtube.com/watch?v=IJ_bVVsQhvc*)

module type PP = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module I = struct
  type t = int

  let pp = Format.pp_print_int
end

module Printer = struct
  type 'a t = 'a -> string

  let string = Fun.id

  let const s = Fun.const s

  let of_pp (type a) (module P : PP with type t = a) x =
    Format.asprintf "%a" P.pp x
  ;;

  let int = of_pp (module I)

  let nl = const "\n"

  open Preface.Fun.Infix

  module Contravariant = Preface.Make.Contravariant.Via_contramap (struct
    type nonrec 'a t = 'a t

    let contramap f x = x % f
  end)

  module Divisible =
    Preface.Make.Divisible.Over_contravariant
      (Contravariant)
      (struct
        type nonrec 'a t = 'a t

        let conquer x = Fun.const "" x

        let divide f x y c =
          let (a, b) = f c in
          x a ^ y b
        ;;
      end)

  module Decidable =
    Preface.Make.Decidable.Over_divisible
      (Divisible)
      (struct
        type nonrec 'a t = 'a t

        let lose f x = Preface.Void.absurd (f x)

        let choose f x y c = Either.fold ~left:x ~right:y (f c)
      end)

  module Infix = struct
    include Contravariant.Infix
    include Divisible.Infix
    include Decidable.Infix
  end
end

module Engine = struct
  type t =
    | Pistons of int
    | Rocket

  let to_either = function
    | Pistons x -> Either.Left x
    | Rocket -> Either.Right ()
  ;;

  let print =
    let open Printer in
    let open Printer.Infix in
    to_either >$< (const "Pistons: " *< int >|< const "Rocket")
  ;;
end

module Car = struct
  type t = {
      make : string
    ; model : string
    ; engine : Engine.t
  }

  let destruct { make; model; engine } = ((make, model), engine)

  let print =
    let open Printer in
    let open Printer.Infix in
    destruct
    >$< ( const "Make: " *< string
        >* nl
        >*< (const "Model: " *< string >* nl)
        >*< Engine.print )
  ;;
end

let toyota_corolla =
  Car.{ make = "Toyota"; model = "Corolla"; engine = Engine.Pistons 4 }
;;

let ford_mustang =
  Car.{ make = "Ford"; model = "Mustang"; engine = Engine.Rocket }
;;

let test_printer_mustang =
  let open Alcotest in
  test_case "Pretty print a Ford Mustang" `Quick (fun () ->
      let exprected = {|Make: Ford
Model: Mustang
Rocket|}
      and computed = Car.print ford_mustang in
      check string "Should be printed" exprected computed )
;;

let test_printer_corolla =
  let open Alcotest in
  test_case "Pretty print a Toyota Corolla" `Quick (fun () ->
      let exprected = {|Make: Toyota
Model: Corolla
Pistons: 4|}
      and computed = Car.print toyota_corolla in
      check string "Should be printed" exprected computed )
;;

let cases =
  [
    ( "Pretty Printer using contravariants functors"
    , [ test_printer_mustang; test_printer_corolla ] )
  ]
;;
