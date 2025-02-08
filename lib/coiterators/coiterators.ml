(** Coiteration 
    coiteration makes it possible to get rid of recursion.
    As a consequence, streams cannot be defined recursively as in
      constant v = cons v (constant v)
    Despite  the pre functions looks like cons, we cannot define
    constant this way because pre changes the type of the state.    
*)

(** The type of concrete streams *)

(* A coiterator is a state monad value and an initial state *)
type ('a,'s) co = Co of ('s -> ('a * 's)) * 's 

(* for testing purpose *)
let from_list (l : 'a list) (a :'a) : ('a, 'a list) co =
    Co ((fun l -> match l with [] -> (a,l) | h::t -> (h, t)), l)
  
let rec to_list (Co (f,i) : ('a,'b) co) (n : int) = 
  if n > 0 then 
    let (a, s') = f i in 
      a::(to_list (Co (f, s')) (n-1))
  else []
    
let co_apply : 
  (('t1 -> 's2 option -> ('t2 * 's2 option), 'sf) co) -> 
  ('t1, 's1) co -> ('t2, ('s2 option * 'sf * 's1)) co =
  fun (Co (tf,sf)) (Co (te, se)) ->
    Co ((fun (st,sf,se) ->
    let (vf, sf') = tf sf in 
    let (ve, se') = te se in
    let (v,st') = vf ve st in
        (v, (st',sf',se'))),
        (None, sf, se))
    
let co_const : 't -> ('t, 's option) co = 
  fun v -> Co ((fun _ -> (v, None)), None) 

let co_extend : ('a -> 'b, 'sf) co -> ('a, 's) co -> ('b, ('sf * 's)) co =
  fun (Co (f, i)) (Co (e, ie)) ->
    Co ((fun (sf,se) -> 
      let (vf,sf') = f sf in
      let (ve,se') = e se in
        ((vf ve), (sf', se'))),
        (i,ie))

let co_plus1 : (int, 'a) co -> (int, 'b) co -> (int, ('c option * 'a) * 'b) co = 
  fun x y -> co_extend (co_extend (co_const (+)) x) y
 
let co_fby : ('a, 's1) co -> ('a, 's2) co -> ('a, ('a option * 's1 * 's2)) co =
  fun (Co (tx, ix)) (Co (ty, iy)) ->
    Co ((fun (init, sx, sy) -> 
          let (vx, sx') = tx sx in
          let (vy, sy') = ty sy in
          match init with 
            | None -> (vx, (Some vy, sx', sy'))
            | Some v -> (v, (Some vy, sx', sy'))),
        (None, ix, iy))

(* let co_apply : (('a -> 's2 option -> ('b * 's2 option)), 'sf) co ->
  ('a,'s1) co -> ('b, ('s2 option * 'sf * 's1)) co =
  fun (Co (f, sf)) (Co (e, se)) ->
    Co ((fun (st, sf, se) -> 
      let (vf, sf') = f sf in
      let (ve, se') = e se in
      let (v, st') = vf ve st in
        (v, (st', sf', se'))),
        (None, sf, se)) *)

let co_lambda : (('a, 's1 option) co -> ('b, 's2) co) ->
  (('a -> 's2 option -> 'b * 's2 option), 'sf option) co = 
  fun f ->
    co_const  (fun v s -> 
      let (Co (t, i)) = f (co_const v) in
     let s1 = (match s with None -> i | Some s2 -> s2) in
     let (v', s') = t s1 in
        (v', Some s'))

(* co_apply (co_lambda (co_apply f)) = co_apply f *)