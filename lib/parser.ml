open Parc.Make (struct
  type c = Lexer.token
end)

type expr_node =
  | Name of string
  | Int of int
  | Infix of string * expr_node * expr_node
  | Lambda of string * expr_node
  | Apply of expr_node * expr_node
  | Branch of expr_node * expr_node * expr_node
  | Binding of string * expr_node * expr_node

type stmt_node = Assign of string * expr_node | Pass
type program = stmt_node list

module Grammar = struct
  (* individual tokens *)
  let newline = next' (function Lexer.Newline -> true | _ -> false)
  let equals = next' (function Lexer.Equals -> true | _ -> false)
  let parenL = next' (function Lexer.ParenL -> true | _ -> false)
  let parenR = next' (function Lexer.ParenR -> true | _ -> false)
  let backslash = next' (function Lexer.Backslash -> true | _ -> false)
  let arrow = next' (function Lexer.Arrow -> true | _ -> false)
  let lit_if = next' (function Lexer.If -> true | _ -> false)
  let lit_then = next' (function Lexer.Then -> true | _ -> false)
  let lit_else = next' (function Lexer.Else -> true | _ -> false)
  let lit_let = next' (function Lexer.Let -> true | _ -> false)
  let lit_in = next' (function Lexer.In -> true | _ -> false)
  let operator = next_of (function Lexer.Operator x -> Some x | _ -> None)
  let name = next_of (function Lexer.Name x -> Some x | _ -> None)
  let intl = next_of (function Lexer.Int x -> Some x | _ -> None)

  (* expressions *)
  let rec expr_head' () =
    laz (fun () ->
        (* Name *)
        (fun x -> Name x)
        <$> name
        (* Int *)
        <|> ((fun x -> Int x) <$> intl)
        (* ( Expr ) *)
        <|> (parenL *> aexpr' () <* parenR))

  (* left-recursive cases for expressions *)
  and expr_tail' () =
    laz (fun () ->
        (* Expr *)
        pure (fun x -> x)
        (* Expr Expr *)
        <|> ((fun rhs lhs -> Apply (lhs, rhs)) <$> aexpr' ())
        (* Expr Op Expr *)
        <|> ((fun op rhs lhs -> Infix (op, lhs, rhs)) <$> operator <*> aexpr' ()))

  and expr' () = expr_head' () <**> expr_tail' ()

  and aexpr' () =
    laz (fun () ->
        expr' ()
        (* \ Name -> Expr  *)
        <|> ((fun x e -> Lambda (x, e)) <$> (backslash *> name <* arrow) <*> aexpr' ())
        (* if Expr then Expr else Expr  *)
        <|> ((fun c e e' -> Branch (c, e, e'))
            <$> lit_if *> expr' ()
            <*> lit_then *> expr' ()
            <*> lit_else *> aexpr' ())
        (* let Name = Expr in Expr  *)
        <|> ((fun x e e' -> Binding (x, e, e'))
            <$> lit_let *> name
            <*> equals *> expr' ()
            <*> lit_in *> aexpr' ()))

  let expr = expr' ()
  let expr_head = expr_head' ()
  let expr_tail = expr_tail' ()
  let aexpr = aexpr' ()

  (* statements *)
  let stmt =
    (fun x _ e _ -> Assign (x, e))
    <$> name <*> equals <*> aexpr <*> newline
    <|> newline *> pure Pass

  let rec prog' () : program f =
    laz (fun () -> pure [] <|> ((fun x xs -> x :: xs) <$> stmt <*> prog' ())) |> finish

  let prog = prog' ()
end

let parse s = determine Grammar.prog s
