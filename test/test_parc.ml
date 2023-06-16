open Vase.Parc
open Vase.Lists

let deterministic p s =
  try
    determine p s |> ignore;
    true
  with Failure _ -> false

let determine' p s = determine p (list_of_string s)
let deterministic' p s = deterministic p (list_of_string s)

let test_cases =
  let open Alcotest in
  [
    ( "Parc",
      [
        test_case "abc <= word \"abc\"" `Quick (fun () ->
            check string "" "abc" (determine' (word "abc") "abc"));
        test_case "abc <= word \"abc\"" `Quick (fun () ->
            check bool "" true (deterministic' (word "abc") "abc"));
        test_case "ab <= word \"abc\"" `Quick (fun () ->
            check bool "" false (deterministic' (word "abc") "ab"));
        test_case "abcd <= word \"abc\"" `Quick (fun () ->
            check bool "" false (deterministic' (word "abc") "abcd"));
        test_case "| <= word \"abc\"*" `Quick (fun () ->
            check (list string) "" [] (determine' (many (word "abc")) ""));
        test_case "| <= word \"abc\"+" `Quick (fun () ->
            check bool "" false (deterministic' (some (word "abc")) ""));
        test_case "abcabc <= word \"abc\"+" `Quick (fun () ->
            check (list string) "" [ "abc"; "abc" ] (determine' (some (word "abc")) ""));
      ] );
  ]
