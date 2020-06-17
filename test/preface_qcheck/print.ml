type 'a t = 'a QCheck.Print.t

let identity pp x =
  let ppi pp ppf x = Format.fprintf ppf "%s" (pp x) in
  Format.asprintf "%a" (Preface_stdlib.Identity.pp (ppi pp)) x
;;

include (QCheck.Print : module type of QCheck.Print with type 'a t := 'a t)
