open Vase

let test_matches (msg, is, what, where) =
  Alcotest.test_case msg `Quick (fun () ->
      Alcotest.(check bool) "matches" is (Regex.matches what where);
      if is then
        Alcotest.(check bool)
          "star-matches-2" true
          (Regex.matches (what @ what) (Regex.star where))
      else ();
      Alcotest.(check bool)
        "complement-matches" (not is)
        (Regex.matches what (Regex.complement where));
      ())

let ( ~^ ) a = Regex.symbols [ a ]
let ( ~^^ ) = Regex.symbols
let ( ~* ) = Regex.star
let ( ~! ) = Regex.complement
let ( +>>+ ) = Regex.concat
let ( +|+ ) = Regex.union
let ( +&+ ) = Regex.intersect
let a_b_star = ~^'a' +>>+ ~*(~^'b')
let ab_or_ac = ~^'a' +>>+ ~^'b' +|+ (~^'a' +>>+ ~^'c')
let a_and_a_star = ~^'a' +&+ ~*(~^'a')

let regex_cases =
  [
    ("| </= 0", false, [], Regex.nothing);
    ("| <= |", true, [], Regex.null);
    ("a <= a", true, [ 'a' ], ~^'a');
    ("b </= a", false, [ 'b' ], ~^'a');
    ("| <= ab*", false, [], a_b_star);
    ("a <= ab*", true, [ 'a' ], a_b_star);
    ("abbb <= ab*", true, [ 'a'; 'b'; 'b'; 'b' ], a_b_star);
    ("aabbb </= ab*", false, [ 'a'; 'a'; 'b'; 'b' ], a_b_star);
    ("ab <= ab+ac", true, [ 'a'; 'b' ], ab_or_ac);
    ("ac <= ab+ac", true, [ 'a'; 'c' ], ab_or_ac);
    ("a </= ab+ac", false, [ 'a' ], ab_or_ac);
    ("a <= a & a*", true, [ 'a' ], a_and_a_star);
    ("| </= a & a*", false, [], a_and_a_star);
    ("aa </= a & a*", false, [ 'a'; 'a' ], a_and_a_star);
    ("aabbb </= !(ab*)", true, [ 'a'; 'a'; 'b'; 'b' ], ~!a_b_star);
    ("cab <= [abc]*", true, [ 'c'; 'a'; 'b' ], ~*(~^^[ 'a'; 'b'; 'c' ]));
    ("cad </= [abc]*", false, [ 'c'; 'a'; 'd' ], ~*(~^^[ 'a'; 'b'; 'c' ]));
  ]

let test_cases = [ ("Regex", List.map test_matches regex_cases) ]
