module type DRIVER = sig
  type input
  type output

  val arbitrary : input QCheck.arbitrary
  val left : input -> output
  val right : input -> output
  val equal : output -> output -> bool
  val name : string
end

module Test (T : DRIVER) = struct
  let test count =
    QCheck.Test.make ~name:T.name ~count T.arbitrary (fun input ->
        let left = T.left input
        and right = T.right input in
        T.equal left right )
  ;;
end
