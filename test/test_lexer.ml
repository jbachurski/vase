open Vase.Lexer

let string_of_list s = s |> List.to_seq |> String.of_seq

let token_pp pf = function
  | Newline -> Format.fprintf pf "\\n"
  | Indent -> Format.fprintf pf "->"
  | Dedent -> Format.fprintf pf "<-"
  | Sep -> Format.fprintf pf ";"
  | ParenL -> Format.fprintf pf "("
  | ParenR -> Format.fprintf pf ")"
  | BracketL -> Format.fprintf pf "["
  | BracketR -> Format.fprintf pf "]"
  | BraceL -> Format.fprintf pf "{"
  | BraceR -> Format.fprintf pf "}"
  | Equals -> Format.fprintf pf "="
  | If -> Format.fprintf pf "if"
  | Then -> Format.fprintf pf "then"
  | Else -> Format.fprintf pf "else"
  | Operator s -> Format.fprintf pf "Operator '%s'" s
  | Name s -> Format.fprintf pf "Name '%s'" s
  | Int d -> Format.fprintf pf "Int '%d'" d

let case1 = "if 42 then\n  halt ()\nelse\n  2++ 2"

let lex1 =
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
  ]

let test_cases =
  let open Alcotest in
  [
    ( "Lexer",
      [
        test_case "lex" `Quick (fun () ->
            check (list (of_pp token_pp)) "" lex1 (lex case1));
      ] );
  ]