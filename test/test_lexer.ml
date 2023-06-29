open Vase.Lexer

let string_of_list s = s |> List.to_seq |> String.of_seq

let cases =
  [
    ("([{}])", [ ParenL; BracketL; BraceL; BraceR; BracketR; ParenR; Newline ]);
    ( "if 42 then\n  halt ()\nelse\n  2++ 2",
      [
        If;
        Int 42;
        Then;
        Newline;
        Indent;
        Name "halt";
        ParenL;
        ParenR;
        Newline;
        Dedent;
        Else;
        Newline;
        Indent;
        Int 2;
        Operator "++";
        Int 2;
        Newline;
        Dedent;
        Newline;
      ] );
  ]

let test_cases =
  let open Alcotest in
  [
    ( "Lexer",
      List.map
        (fun (what, exp) ->
          test_case "lex" `Quick (fun () ->
              check (list (of_pp token_pp)) "" exp (lex what)))
        cases );
  ]
