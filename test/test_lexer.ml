open Vase.Lexer

let string_of_list s = s |> List.to_seq |> String.of_seq

let token_pp pf = function
  | Newline -> Format.fprintf pf "Newline"
  | Indent -> Format.fprintf pf "Indent"
  | Dedent -> Format.fprintf pf "Dedent"
  | Sep -> Format.fprintf pf "Sep"
  | Equals -> Format.fprintf pf "Equals"
  | Operator s -> Format.fprintf pf "Operator %s" (string_of_list s)
  | Name s -> Format.fprintf pf "Name %s" (string_of_list s)
  | IntLiteral s -> Format.fprintf pf "IntLiteral %s" (string_of_list s)
  | FloatLiteral s -> Format.fprintf pf "FloatLiteral %s" (string_of_list s)

let test_cases =
  let open Alcotest in
  [
    ( "Lexer",
      [
        test_case "lex" `Quick (fun () ->
            check
              (list (of_pp token_pp))
              ""
              [ Newline; Indent; Newline; Dedent; Newline; Indent; Newline ]
              (lex "if 1\n  halt\nelse\n  halt"));
      ] );
  ]
