type token =
  | Newline
  | Indent
  | Dedent
  | Sep
  | Equals
  | ParenL
  | ParenR
  | BracketL
  | BracketR
  | BraceL
  | BraceR
  | Backslash
  | Arrow
  | If
  | Then
  | Else
  | Let
  | In
  | Operator of string
  | Name of string
  | Int of int

let token_pp pf = function
  | Newline -> Format.fprintf pf "\\n"
  | Indent -> Format.fprintf pf ">>"
  | Dedent -> Format.fprintf pf "<<"
  | Sep -> Format.fprintf pf ";"
  | Equals -> Format.fprintf pf "="
  | ParenL -> Format.fprintf pf "("
  | ParenR -> Format.fprintf pf ")"
  | BracketL -> Format.fprintf pf "["
  | BracketR -> Format.fprintf pf "]"
  | BraceL -> Format.fprintf pf "{"
  | BraceR -> Format.fprintf pf "}"
  | Backslash -> Format.fprintf pf "\\"
  | Arrow -> Format.fprintf pf "->"
  | If -> Format.fprintf pf "if"
  | Then -> Format.fprintf pf "then"
  | Else -> Format.fprintf pf "else"
  | Let -> Format.fprintf pf "let"
  | In -> Format.fprintf pf "in"
  | Operator s -> Format.fprintf pf "Operator '%s'" s
  | Name s -> Format.fprintf pf "Name '%s'" s
  | Int d -> Format.fprintf pf "Int '%d'" d

let whitespace = [ ' '; '\t'; '\n' ]
let lowercase_letters = Lists.ascii_range 'a' 'z'
let uppercase_letters = Lists.ascii_range 'A' 'Z'
let letters = lowercase_letters @ uppercase_letters
let nonzero_digits = Lists.ascii_range '1' '9'
let digits = '0' :: nonzero_digits
let op_chars = [ '+'; '-'; '*'; '/'; '!'; '@'; '$'; '%'; '^'; '=' ]
let id_chars = letters @ digits @ [ '_' ]
let re_plus r = Regex.concat r (Regex.star r)

let token_grammar =
  let open Re in
  [
    (~|^|';', fun _ -> Sep);
    (~|^|'=', fun _ -> Equals);
    (~|^|'(', fun _ -> ParenL);
    (~|^|')', fun _ -> ParenR);
    (~|^|'[', fun _ -> BracketL);
    (~|^|']', fun _ -> BracketR);
    (~|^|'{', fun _ -> BraceL);
    (~|^|'}', fun _ -> BraceR);
    (~|^|'\\', fun _ -> Backslash);
    (~|>|"->", fun _ -> Arrow);
    (~|>|"if", fun _ -> If);
    (~|>|"then", fun _ -> Then);
    (~|>|"else", fun _ -> Else);
    (~|>|"let", fun _ -> Let);
    (~|>|"in", fun _ -> In);
    (* [OP][OP]* *)
    (~|^^|op_chars |>>| ~|*|(~|^^|op_chars), fun s -> Operator s);
    (* [a-zA-Z_][a-zA-Z0-9_]*'* *)
    ( ~|^^|(letters @ [ '_' ]) |>>| ~|*|(~|^^|id_chars) |>>| ~|*|(~|^^|[ '\'' ]),
      fun s -> Name s );
    (* 0|[1-9][0-9]* *)
    ( ~|^|'0' |+| (~|^^|nonzero_digits |>>| ~|*|(~|^^|digits)),
      fun s -> Int (int_of_string s) );
  ]

(* Performs lexing on a single line *)
let rec lex_line l i0 =
  let init_lexes =
    let open Re in
    (* match [ ]*(TOKEN)[ ]*, where [ ] is whitespace *)
    List.map
      (fun (r, f) ->
        let ws = ~|^^|whitespace in
        (~|*|ws |>>| (r |>>| ~|*|ws), f))
      token_grammar
  in
  let advance a lexes = List.map (fun (r, f) -> (Regex.derivative a r, f)) lexes in
  let all_fail lexes = List.for_all (fun (r, _) -> r = Regex.nothing) lexes in
  let get_done lexes = List.find_opt (fun (r, _) -> Regex.nullable r) lexes in
  let rec go xs lexes i acc =
    (* print_endline ("'" ^ (acc |> List.rev |> Lists.string_of_list) ^ "'"); *)
    let fail () =
      let i' = i - List.length acc in
      raise (Failure (Printf.sprintf "Bad token at line %d, col %d" l i'))
    in
    match xs with
    | [] -> fail ()
    | x :: xs ->
        if all_fail (advance x lexes) then
          match get_done lexes with
          | Some (_, f) ->
              f (acc |> List.rev |> Lists.string_of_list |> String.trim)
              :: lex_line l i (x :: xs)
          | None -> fail ()
        else go xs (advance x lexes) (i + 1) (x :: acc)
  in
  function [] -> [] | [ '\x00' ] -> [] | x :: xs -> go (x :: xs) init_lexes i0 []

let split_on_indentation s =
  s |> Lists.list_of_string |> Lists.span (fun x -> x = ' ' || x = '\t')

let%test "split_on_indentation" =
  split_on_indentation " \tab  " = ([ ' '; '\t' ], [ 'a'; 'b'; ' '; ' ' ])

let indent_diff_tokens d =
  if d < 0 then [ Newline; Indent ]
  else if d > 0 then [ Newline; Dedent ]
  else [ Newline ]

let sgn x = if x > 0 then 1 else if x < 0 then -1 else 0
let rec sum = function [] -> 0 | x :: xs -> x + sum xs

(*
   Accepts a multi-line source.
   Adds extra-regular Indent & Dedent tokens to describe indentation levels.
*)
let lex source =
  let lines = String.split_on_char '\n' source |> List.map split_on_indentation in
  let indents = List.map fst lines in
  if List.hd indents != [] then raise (Failure "Empty source")
  else
    let indent_pairs = List.combine (Lists.drop_last indents) (List.tl indents) in
    let indent_diffs =
      List.map (fun (xs, ys) -> Option.get (Lists.prefix_compare xs ys)) indent_pairs
    in
    let footer = List.init (-sum (List.map sgn indent_diffs)) (fun _ -> Dedent) in
    (List.combine
       (List.mapi (fun l s -> lex_line l 0 (s @ [ '\n'; '\x00' ])) (List.map snd lines))
       (indent_diffs @ [ 0 ])
    |> List.map (fun (xs, d) -> xs @ indent_diff_tokens d)
    |> List.concat)
    @ if footer = [] then [] else footer @ [ Newline ]
