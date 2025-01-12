open Arrows 

type 'a cell

val mkref : 'a -> 'a cell

val get : 'r cell ->  ('a, 'a * 'r) sf 

val set : 'r cell -> ('a * 'r, 'a) sf