let pp_of_print printer ppf x = Format.fprintf ppf "%s" (printer x)

let with_alcotest ?colors ?verbose ?long ~count list =
  Stdlib.List.map
    (fun (name, suite) ->
      ( name
      , Stdlib.List.map
          (QCheck_alcotest.to_alcotest ?colors ?verbose ?long)
          (suite ~count) ) )
    list
;;
