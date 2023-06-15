open Regex

let ( ~|^^| ) = symbols
let ( ~|^| ) x = symbols [ x ]
let ( ~|*| ) = star
let ( ~|!| ) = complement
let ( |>>| ) = concat
let ( |+| ) = union
let ( |&| ) = intersect
let ( ~|>| ) s = List.fold_right concat (List.map ( ~|^| ) (Lists.list_of_string s)) null
