type token =
  | Newline
  | Indent
  | Dedent
  | Sep
  | Equals
  | Operator of char list
  | Name of char list
  | IntLiteral of char list
  | FloatLiteral of char list

let letters = ()

let grammar =
  let open Regex in
  [
    (symbols [ '\n'; ';' ], fun _ -> Sep);
    (symbols [ '=' ], fun _ -> Equals);
    (star (symbols [ '+'; '-'; '*'; '/'; '$' ]), fun s -> Operator s);
    (symbols [], fun s -> Name s);
    (symbols [], fun s -> IntLiteral s);
    (symbols [], fun s -> FloatLiteral s);
  ]

(* Performs lexing on a single line *)
let lex_line _ = []

let split_on_indentation s =
  s |> Lists.list_of_string |> Lists.span (fun x -> x = ' ' || x = '\t')

let%test "split_on_indentation" =
  split_on_indentation " \tab  " = ([ ' '; '\t' ], [ 'a'; 'b'; ' '; ' ' ])

let indent_diff_tokens d =
  if d < 0 then [ Newline; Indent ]
  else if d > 0 then [ Newline; Dedent ]
  else [ Newline ]

let print_ints xs =
  List.map
    (fun x ->
      print_int x;
      print_string "; ")
    xs
  |> ignore

let sgn x = if x > 0 then 1 else if x < 0 then -1 else 0
let rec sum = function [] -> 0 | x :: xs -> x + sum xs

(*
   Accepts a multi-line source.
   Adds extra-regular Indent & Dedent tokens to describe indentation levels.
*)
let lex source =
  let lines = String.split_on_char '\n' source |> List.map split_on_indentation in
  let indents = List.map fst lines in
  if List.hd indents != [] then raise (Failure "xd")
  else
    let indent_pairs = List.combine (Lists.drop_last indents) (List.tl indents) in
    let indent_diffs =
      List.map (fun (xs, ys) -> Option.get (Lists.prefix_compare xs ys)) indent_pairs
    in
    let footer = List.init (-sum (List.map sgn indent_diffs)) (fun _ -> 0) in
    print_ints indent_diffs;
    List.combine (List.map lex_line (List.map snd lines)) (indent_diffs @ footer)
    |> List.map (fun (xs, d) -> xs @ indent_diff_tokens d)
    |> List.concat
