open Parc.Make (struct
  type c = Lexer.token
end)

type expr_node = Infix of string * expr_node * expr_node | Name of string | Int of int
type stmt_node = Assign of string * expr_node | Pass
type program = stmt_node list

module Grammar = struct
  (* individual tokens *)
  let newline = next' (function Lexer.Newline -> true | _ -> false)
  let equals = next' (function Lexer.Equals -> true | _ -> false)
  let parenL = next' (function Lexer.ParenL -> true | _ -> false)
  let parenR = next' (function Lexer.ParenR -> true | _ -> false)
  let operator = next_of (function Lexer.Operator x -> Some x | _ -> None)
  let name = next_of (function Lexer.Name x -> Some x | _ -> None)
  let intl = next_of (function Lexer.Int x -> Some x | _ -> None)

  (* expressions *)
  let expr_head = (fun x -> Name x) <$> name <|> ((fun x -> Int x) <$> intl)

  let rec expr_tail' () =
    laz (fun () ->
        pure (fun x -> x)
        <|> ((fun op rhs lhs -> Infix (op, lhs, rhs)) <$> operator <*> expr' ()))

  and expr' () = expr_head <**> expr_tail' ()

  let expr = expr' ()
  let expr_tail = expr_tail' ()

  (* statements *)
  let stmt =
    (fun x _ e _ -> Assign (x, e))
    <$> name <*> equals <*> expr <*> newline
    <|> newline *> pure Pass

  let rec prog' () : program f =
    laz (fun () -> pure [] <|> ((fun x xs -> x :: xs) <$> stmt <*> prog' ())) |> finish

  let prog = prog' ()
end

let parse s = determine Grammar.prog s
