let () =
  let open Alcotest in
  run "Vase"
    (List.concat [ Test_regex.test_cases; Test_lexer.test_cases; Test_parser.test_cases ])
