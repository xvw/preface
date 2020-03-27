open Preface_core.Fun
open Preface_stdlib.Option
module Free_option = Preface_make.Free.Via_functor (Functor)
open Free_option

let none = lift None

let some v = compose_rigth_to_left lift (fun v -> Some v) v
