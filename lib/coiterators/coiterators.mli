type ('a,'s) co = 
  | Co of ('s -> ('a * 's)) * 's 

val from_list : 'a list -> 'a -> ('a, 'a list) co

val to_list :  ('a,'s) co -> int -> 'a list