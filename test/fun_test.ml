open Preface.Fun

let should_compose_right_to_left () =
    let expected = "42"
    and computed = compose_right_to_left string_of_int ((+) 1) 41
    in  Alcotest.(check string)
            "should_compose_right_to_left"
            expected
            computed

let should_compose_right_to_left_with_infix_operator () =
    let expected = "42"
    and computed = (string_of_int <% ((+) 1)) 41
    in  Alcotest.(check string)
            "should_compose_right_to_left_with_infix_operator"
            expected
            computed

let should_compose_left_to_right() =
    let expected = "42"
    and computed = compose_left_to_right ((+) 1) string_of_int 41
    in  Alcotest.(check string)
            "should_compose_left_to_right"
            expected
            computed

let should_compose_left_to_right_with_infix_operator() =
    let expected = "42"
    and computed = ((+) 1 %> string_of_int) 41
    in  Alcotest.(check string)
            "should_compose_left_to_right_with_infix_operator"
            expected
            computed

let test_cases =
    let open Alcotest in
        "Fun",
        [
            test_case "Right to left composition"
                `Quick should_compose_left_to_right;
            test_case "Right to left infix composition"
                `Quick should_compose_left_to_right_with_infix_operator;
            test_case "Left to right composition"
                `Quick should_compose_right_to_left;
            test_case "Left to right infix composition"
                `Quick should_compose_right_to_left_with_infix_operator;
        ]