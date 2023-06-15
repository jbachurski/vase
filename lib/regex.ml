(* Based on "Regular expressions re-examined" *)

(*
    Invariant: always in similarity-canonical form
    => structural equality is similarity as in the paper
*)

type 'a regex =
  | Nothing (* empty set*)
  | Null (* empty string *)
  | Symbols of 'a list (* character class - sorted *)
  | Star of 'a regex
  | Complement of 'a regex
  | Concat of 'a regex * 'a regex
  | Union of 'a regex * 'a regex
  | Intersect of 'a regex * 'a regex

let regex_op_tag = function
  | Nothing -> 0
  | Null -> 1
  | Symbols _ -> 2
  | Star _ -> 3
  | Complement _ -> 4
  | Concat _ -> 5
  | Union _ -> 6
  | Intersect _ -> 7

let lex_compare a b = if a = 0 then b else a

let rec regex_compare r s =
  match (r, s) with
  | Nothing, Nothing -> 0
  | Null, Null -> 0
  | Symbols xs, Symbols xs' -> compare xs xs'
  | Star r', Star s' -> regex_compare r' s'
  | Complement r', Complement s' -> regex_compare r' s'
  | Concat (r1, r2), Concat (s1, s2) ->
      lex_compare (regex_compare r1 s1) (regex_compare r2 s2)
  | Union (r1, r2), Union (s1, s2) ->
      lex_compare (regex_compare r1 s1) (regex_compare r2 s2)
  | Intersect (r1, r2), Intersect (s1, s2) ->
      lex_compare (regex_compare r1 s1) (regex_compare r2 s2)
  | _ -> compare (regex_op_tag r) (regex_op_tag s)

let regex_of_bool = function false -> Nothing | true -> Null

(* Utilities for regex binary operators *)
type regex_bin = ConcatTag | UnionTag | IntersectTag

let regex_bin_case = function
  | Concat (r, s) -> Some (ConcatTag, (r, s))
  | Union (r, s) -> Some (UnionTag, (r, s))
  | Intersect (r, s) -> Some (IntersectTag, (r, s))
  | _ -> None

(* Convert subtree spanned by operator t into a list *)
let rec regex_bin_unchain t r' =
  match regex_bin_case r' with
  | Some (t', (r, rs)) when t = t' -> r :: regex_bin_unchain t rs
  | _ -> [ r' ]

(* Convert list back into a canonical application of t *)
let regex_bin_chain t =
  let f x y =
    match t with
    | ConcatTag -> Concat (x, y)
    | UnionTag -> Union (x, y)
    | IntersectTag -> Intersect (x, y)
  in
  let rec go = function r, s :: rs -> f r (go (s, rs)) | r, [] -> r in
  (* create a list like Concat (x, Concat (y, Concat (z, ...))) *)
  function
  | r :: rs -> go (r, rs)
  (* we shouldn't hit this case, as the un-chains are always non-empty *)
  | [] -> (
      match t with
      | ConcatTag -> Null
      | UnionTag -> Nothing
      | IntersectTag -> Complement Nothing)

let regex_bin_make_assoc t r s =
  let rs = regex_bin_unchain t r in
  let ss = regex_bin_unchain t s in
  regex_bin_chain t (List.merge regex_compare rs ss)

let regex_bin_make_non_assoc t r s =
  let rs = regex_bin_unchain t r in
  let ss = regex_bin_unchain t s in
  regex_bin_chain t (rs @ ss)

(* Exported smart regex constructors which assume and retain canonical form *)

let nothing = Nothing
let null = Null
let symbols xs = Symbols (List.sort compare xs)
let star = function Star r -> Star r | Null -> Null | Nothing -> Nothing | r -> Star r
let complement = function Complement r -> r | r -> Complement r

let concat r' s' =
  match (r', s') with
  | Nothing, _ -> Nothing
  | _, Nothing -> Nothing
  | Null, r -> r
  | r, Null -> r
  | _ -> regex_bin_make_non_assoc ConcatTag r' s'

let union r' s' =
  match (r', s') with
  | Nothing, r -> r
  | r, Nothing -> r
  | _, Complement Nothing -> Complement Nothing
  | Complement Nothing, _ -> Complement Nothing
  | Symbols xs, Symbols xs' -> Symbols (List.merge compare xs xs')
  | _ -> regex_bin_make_assoc UnionTag r' s'

let intersect r' s' =
  match (r', s') with
  | Nothing, _ -> Nothing
  | _, Nothing -> Nothing
  | r, Complement Nothing -> r
  | Complement Nothing, r -> r
  | _ -> regex_bin_make_assoc IntersectTag r' s'

(* Brzozowski derivatives *)

let rec nullable = function
  | Nothing -> false
  | Null -> true
  | Symbols _ -> false
  | Star _ -> true
  | Complement r -> not (nullable r)
  | Concat (r, s) -> nullable r && nullable s
  | Union (r, s) -> nullable r || nullable s
  | Intersect (r, s) -> nullable r && nullable s

let rec derivative a = function
  | Nothing -> Nothing
  | Null -> Nothing
  | Symbols xs when List.exists (fun a' -> a = a') xs -> Null
  | Symbols _ -> Nothing
  | Star r -> concat (derivative a r) (star r)
  | Complement r -> complement (derivative a r)
  | Concat (r, s) ->
      union
        (concat (derivative a r) s)
        (concat (regex_of_bool (nullable r)) (derivative a s))
  | Union (r, s) -> union (derivative a r) (derivative a s)
  | Intersect (r, s) -> intersect (derivative a r) (derivative a s)

let flip f x y = f y x
let derivatives xs r = List.fold_left (flip derivative) r xs
let matches xs r = nullable (derivatives xs r)
