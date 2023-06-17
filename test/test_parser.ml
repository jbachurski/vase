open Vase.Lists

open Vase.Parc.Make (struct
  type c = char
end)

open Vase.Parser

let deterministic p s =
  try
    determine p s |> ignore;
    true
  with Failure _ -> false

let determine' p s = determine p (list_of_string s)
let deterministic' p s = deterministic p (list_of_string s)
let word' p = string_of_list <$> word (list_of_string p)

let test_cases =
  let open Alcotest in
  [
    ( "Parc",
      [
        test_case "abc <= word \"abc\"" `Quick (fun () ->
            check string "" "abc" (determine' (word' "abc") "abc"));
        test_case "abc <= word \"abc\"" `Quick (fun () ->
            check bool "" true (deterministic' (word' "abc") "abc"));
        test_case "ab <= word \"abc\"" `Quick (fun () ->
            check bool "" false (deterministic' (word' "abc") "ab"));
        test_case "abcd <= word \"abc\"" `Quick (fun () ->
            check bool "" false (deterministic' (word' "abc") "abcd"));
        test_case "| <= word \"abc\"*" `Quick (fun () ->
            check (list string) "" [] (determine' (many (word' "abc")) ""));
        test_case "| <= word \"abc\"+" `Quick (fun () ->
            check bool "" false (deterministic' (some (word' "abc")) ""));
        test_case "abc <= word \"abc\"+" `Quick (fun () ->
            check (list string) "" [ "abc" ] (determine' (some (word' "abc")) "abc"));
        test_case "abcabc <= word \"abc\"+" `Quick (fun () ->
            check (list string) "" [ "abc"; "abc" ]
              (determine' (some (word' "abc")) "abcabc"));
        test_case "program" `Quick (fun () ->
            check bool "" true ([ Assign ("x", Int 42) ] = parse (Vase.Lexer.lex "x = 42")));
        test_case "program" `Quick (fun () ->
            check bool "" true
              ([ Assign ("y", Infix ("+", Int 17, Name "x")) ]
              = parse (Vase.Lexer.lex "y = 17 + x")));
        test_case "program" `Quick (fun () ->
            check bool "" true
              ([ Assign ("x", Int 42); Assign ("y", Infix ("+", Int 17, Name "x")) ]
              = parse (Vase.Lexer.lex "x = 42\ny = 17 + x")));
        test_case "program" `Quick (fun () ->
            check bool "" true
              ([
                 Assign
                   ("a", Infix ("+", Int 1, Infix ("+", Int 2, Infix ("+", Int 3, Int 4))));
               ]
              = parse (Vase.Lexer.lex "a = 1 + 2 + 3 + 4")));
      ] );
  ]
