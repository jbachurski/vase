open Vase

let test_ab () =
  Alcotest.(check bool)
    "matches" true
    (Regex.matches []
       (Regex.concat (Regex.star (Regex.symbol 'a')) (Regex.star (Regex.symbol 'b'))))

let () =
  let open Alcotest in
  run "Regex" [ ("matches", [ test_case "a*b*" `Quick test_ab ]) ]
