let list_of_string s = s |> String.to_seq |> List.of_seq
let%test "list_of_string" = list_of_string "abc" = [ 'a'; 'b'; 'c' ]

let rec span f = function
  | [] -> ([], [])
  | x :: xs when f x ->
      let hs, ts = span f xs in
      (x :: hs, ts)
  | xs -> ([], xs)

let%test "span" = span (fun x -> x > 0) [ 1; 1; 0; 1 ] = ([ 1; 1 ], [ 0; 1 ])
let len = List.length
let%test "len" = len [ 1; 2; 3 ] = 3
let rec take n = function x :: xs when n > 0 -> x :: take (n - 1) xs | _ -> []
let rec drop n = function _ :: xs when n > 0 -> drop (n - 1) xs | xs -> xs
let%test "take" = take 2 [ 1; 2; 3 ] = [ 1; 2 ]
let drop_last xs = take (len xs - 1) xs
let is_prefix xs ys = xs = take (len xs) ys
let%test "is_prefix" = is_prefix [ 1; 2 ] [ 1; 2; 3 ]
let%test "is_prefix" = is_prefix [ 1; 3 ] [ 1; 2; 3 ] |> not
let either_is_prefix xs ys = if len xs < len ys then is_prefix xs ys else is_prefix ys xs
let%test "either_is_prefix" = either_is_prefix [ 1; 2 ] [ 1; 2; 3 ]
let%test "either_is_prefix" = either_is_prefix [ 1; 2; 3 ] [ 1; 2 ]
let%test "either_is_prefix" = either_is_prefix [ 1; 2; 3 ] [ 1; 3 ] |> not

let prefix_compare xs ys =
  if not (either_is_prefix xs ys) then None else Some (compare (len xs) (len ys))

let%test "prefix_compare" = prefix_compare [ 1; 2 ] [ 1; 2; 3 ] = Some (-1)
let%test "prefix_compare" = prefix_compare [ 1; 2; 3 ] [ 1; 2 ] = Some 1
let%test "prefix_compare" = prefix_compare [ 1; 2 ] [ 1; 2 ] = Some 0
let%test "prefix_compare" = prefix_compare [ 1; 2 ] [ 1; 3 ] = None
