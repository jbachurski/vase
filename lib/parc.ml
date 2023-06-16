(* Parser combinators *)

include Monad

type str = char list
type 'a parser = str -> ('a * str) Seq.t

let parser_fmap (f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun s -> Seq.map (fun (a, s') -> (f a, s')) (p s)

let parser_apply (f : ('a -> 'b) parser) (p : 'a parser) : 'b parser =
 fun s -> Seq.concat_map (fun (g, s') -> parser_fmap g p s') (f s)

module Parser_Functor_base : Functor_base with type 'a f = 'a parser = struct
  type 'a f = 'a parser

  let ( <$> ) = parser_fmap
end

module Parser_Applicative_base : Applicative_base with type 'a f = 'a parser = struct
  type 'a f = 'a parser

  let pure a s = List.to_seq [ (a, s) ]
  let ( <*> ) f a s = parser_apply f a s
end

include Applicative (Parser_Functor_base) (Parser_Applicative_base)

let filter f (p : 'a parser) : 'a parser = fun s -> Seq.filter f (p s)
let failure : 'a parser = fun _ -> Seq.empty
let just a : 'a parser = fun s -> [ (a, s) ] |> List.to_seq

let determine (p : 'a parser) s =
  match filter (fun (_, s') -> s' = []) p s () with
  | Seq.Cons ((a, _), r) -> (
      match r () with
      | Seq.Cons _ -> raise (Failure "Nondeterministic parse (many parses)")
      | Seq.Nil -> a)
  | Seq.Nil -> raise (Failure "Parse failed (no complete parse)")

let word p : 'a parser =
  let open Lists in
  let p' = list_of_string p in
  fun s -> (if is_prefix p' s then [ (p, drop (len p') s) ] else []) |> List.to_seq

let ( <|> ) (p : 'a parser) (q : 'a parser) : 'a parser = fun s -> Seq.append (p s) (q s)

(* we don't eta-reduce here to avoid infinite recursion with some *)
let rec many p s = (just [] <|> some p) s
and some p = (fun x xs -> x :: xs) <$> p <*> many p

let optional p = just None <|> ((fun x -> Some x) <$> p)
