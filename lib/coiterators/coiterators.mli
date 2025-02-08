type ('a,'s) co = 
  | Co of ('s -> ('a * 's)) * 's 

val from_list : 'a list -> 'a -> ('a, 'a list) co

val to_list :  ('a,'s) co -> int -> 'a list
    
val co_const : 't -> ('t, 's option) co  

val co_extend : ('a -> 'b, 'sf) co -> ('a, 's) co -> ('b, ('sf * 's)) co

val co_plus1 : (int, 'a) co -> (int, 'b) co -> (int, ('c option * 'a) * 'b) co

val co_fby : ('a, 's1) co -> ('a, 's2) co -> ('a, ('a option * 's1 * 's2)) co 


(* transform a stream function into function iterator *)
(* makes the state explicit *)

val co_apply : (('a -> 's2 option -> ('b * 's2 option)), 'sf) co ->
  ('a,'s1) co -> ('b, ('s2 option * 'sf * 's1)) co

val co_lambda : (('a, 's1 option) co -> ('b, 's2) co) ->
  (('a -> 's2 option -> 'b * 's2 option), 'sf option) co

(* build from a iterator ... *)

(* co_apply (co_lambda (co_apply f)) = co_apply f *)