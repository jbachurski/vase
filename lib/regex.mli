type 'a regex

(* constructors *)
val nothing : 'a regex
val null : 'a regex
val symbols : 'a list -> 'a regex
val star : 'a regex -> 'a regex
val complement : 'a regex -> 'a regex
val concat : 'a regex -> 'a regex -> 'a regex
val union : 'a regex -> 'a regex -> 'a regex
val intersect : 'a regex -> 'a regex -> 'a regex

(* regex operations *)
val matches : 'a list -> 'a regex -> bool
