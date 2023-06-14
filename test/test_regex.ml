open Vase

let test_matches (msg, is, what, where) =
  Alcotest.test_case msg `Quick (fun () ->
      Alcotest.(check bool) "matches" is (Regex.matches what where))

let a_b_star = Regex.concat (Regex.symbol 'a') (Regex.star (Regex.symbol 'b'))

let () =
  let open Alcotest in
  run "Regex"
    [
      ( "matches",
        List.map test_matches
          [
            ("| </= 0", false, [], Regex.nothing);
            ("| <= |", true, [], Regex.null);
            ("a <= a", true, [ 'a' ], Regex.symbol 'a');
            ("b </= a", false, [ 'b' ], Regex.symbol 'a');
            ("| <= ab*", false, [], a_b_star);
            ("a <= ab*", true, [], a_b_star);
            ("abbb <= ab*", true, [], a_b_star);
            ("aabbb </= ab*", false, [], a_b_star);
          ] );
    ]
