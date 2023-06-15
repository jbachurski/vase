(* Based on "Regular expressions re-examined" *)

(*
    Invariant: always in similarity-canonical form
    => structural equality is similarity as in the paper
*)
type 'a regex =
  | Nothing (* empty set*)
  | Null (* empty string *)
  | Symbol of 'a
  | Star of 'a regex
  | Complement of 'a regex
  | Concat of 'a regex * 'a regex
  | Union of 'a regex * 'a regex
  | Intersect of 'a regex * 'a regex

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

let regex_bin_make t r s =
  let rs = regex_bin_unchain t r in
  let ss = regex_bin_unchain t s in
  regex_bin_chain t (List.merge compare rs ss)

(* Exported smart regex constructors which assume and retain canonical form *)

let nothing = Nothing
let null = Null
let symbol a = Symbol a
let star = function Star r -> Star r | Null -> Null | Nothing -> Nothing | r -> Star r
let complement = function Complement r -> r | r -> Complement r

let concat r' s' =
  match (r', s') with
  | Nothing, _ -> Nothing
  | _, Nothing -> Nothing
  | Null, r -> r
  | r, Null -> r
  | _ -> regex_bin_make ConcatTag r' s'

let union r' s' =
  match (r', s') with
  | Nothing, r -> r
  | r, Nothing -> r
  | _, Complement Nothing -> Complement Nothing
  | Complement Nothing, _ -> Complement Nothing
  | _ -> regex_bin_make UnionTag r' s'

let intersect r' s' =
  match (r', s') with
  | Nothing, _ -> Nothing
  | _, Nothing -> Nothing
  | r, Complement Nothing -> r
  | Complement Nothing, r -> r
  | _ -> regex_bin_make IntersectTag r' s'

(* Brzozowski derivatives *)

let rec nullable = function
  | Nothing -> false
  | Null -> true
  | Symbol _ -> false
  | Star _ -> true
  | Complement r -> not (nullable r)
  | Concat (r, s) -> nullable r && nullable s
  | Union (r, s) -> nullable r || nullable s
  | Intersect (r, s) -> nullable r && nullable s

let rec derivative a = function
  | Nothing -> Nothing
  | Null -> Nothing
  | Symbol a' when a = a' -> Null
  | Symbol _ -> Nothing
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
