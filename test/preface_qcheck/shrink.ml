type 'a t = 'a QCheck.Shrink.t

let identity shrink value =
  value
  |> Preface_stdlib.Identity.Functor.map (fun x ->
         x |> shrink |> QCheck.Iter.map Preface_stdlib.Identity.pure)
  |> Preface_stdlib.Identity.extract
;;

include (QCheck.Shrink : module type of QCheck.Shrink with type 'a t := 'a t)
