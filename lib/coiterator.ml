open Format
open Util

(** Coiteration 
    coiteration makes it possible to get rid of recursion.
    As a consequence, streams cannot be defined recursively as in
      constant v = cons v (constant v)
    Despite  the pre functions looks like cons, we cannot define
    constant this way because pre changes the type of the state.    
*)

(** The type of concrete streams *)
type ('a,'s) co = 
  | Co of ('s -> ('a * 's)) * 's 

(** head of a concrete stream *)
let head (Co (f,s) : ('a,'b) co) : 'a = fst (f s)

(** tail of a concrete stream *)
let tail (Co (f,s) : ('a,'b) co) : ('a,'b) co = 
  Co (f,(snd (f s)))
  
(** The usual coalgebra operator for the functor F X = A * X *)
let destr (c : ('a, 's) co) : 'a * ('a,'s) co = 
  (head c, tail c)

(** the stream of natural numbers *)

(** the constant stream *)
let constant (c : 'a) : ('a, 's) co =
  Co ((fun () -> (c,())), ())
(** - head (constant a) = a 
    - tail (constant a) = constant a *)

(** mapping *)
let map (f : 'a -> 'b) : (('a, 'c) co -> ('b, 'c) co) = 
  fun (Co (g, s)) -> 
    Co ((fun s -> let (b, g') = g s in (f b, g')), s)
(** - head (map f c) = f c 
    - tail (map f c) = map f (tail c) *)
  
(** pre *)
let pre (v : 'a) (Co (f, s) : ('a, 's) co) : ('a, ('a * 's)) co = 
  Co ((fun (x, s) -> (x, f s)), (v, s))
(** 
  - head (prev v c) = v 
  - tail (prev v c) = c *)

let plus (Co (f1, s1) : (int,'s1) co) (Co (f2, s2) : (int, 's2) co) : (int, 's1 * 's2) co = 
  Co ((fun (s1, s2) -> 
    let (x, s1') = f1 s1 and (y, s2') = f2 s2 in 
      (x + y, (s1', s2'))), (s1, s2))
(** 
  - head (plus c1 c2) = head c1 + head c2 
  - tail (plus c1 c2 = plus (tail c1) (tail c2) --> map, applicative functor *)

let even (Co (f, s) : ('a,'s) co) : ('a, 's) co =
  Co ((fun s -> f (snd (f s))), s)
(** even 
  - head (even c) = head c 
  - tail (even c) = even (tail (tail c)) *)

(** arrow operators *)
(* maybe some problems with connections because of the state *)

(* is it possible to define functions with co to co as parameter *)

(* composition *)

let zip (Co (f, s1) : ('a,'b) co) (Co (g,s2) : ('c,'d) co) : ('a *'c, 'b * 'd) co =
  Co ((fun (s1,s2) -> let (b,s1') = f s1 in let (d,s2') = g s2 in ((b,d), (s1',s2'))), (s1,s2))



let compose 
  (f : ('a,'s1) co -> ('b, 's3) co)
  (g : ('b, 's3) co -> ('c, 's4) co) : ('a,'s1) co -> ('c,'s4) co =
  fun c -> g (f c)

  (* state weakening for composition *)

let fork (Co (f,s) : ('a,'s) co) : (('a * 'a), 's) co =
  Co ((fun s -> let (a,s') = f s in ((a,a), s')), s)

let floats_of_ints (Co (f, s) : (int, 's) co) : (float, 's) co =
  Co ((fun s -> let (x,y) = f s in (float_of_int x, y)), s)

let value (f : 's -> 'a * 's) (s : 's) = fst (f s)
let next (f : 's -> 'a * 's) (s : 's) = snd (f s)

(* arrows *) 
(* acts as the identity on the state *)

(* important : all operators produces boxes whose output state has the same kind as the input state*)
(* As expected, only the loop function changes the structure of the state 
  by adding the new 'memory to it' *)
(* operators open box to rearrange the plugins *)

let arr : ('a -> 'b) -> ('a, 'c) co -> ('b, 'c) co = map

(* let arr (f : 'a -> 'b) :  ('a, 'c) co -> ('a, 'c) co = map f *)

(* f must be polymorphic to apply to each possible state *)
let first (f : ('a, 's) co -> ('b, 's) co) : ('a * 'c, 's) co -> ('b * 'c, 's) co = 
  fun (Co (h, s) : ('a * 'c, 's) co)  ->  
    let h' : ('a, 's) co = Co ((fun s -> let ((x,_),y) = h s in (x, y)), s) in 
    let (Co (h'', _) : ('b, 's) co) = f h' in
      Co ((fun s -> let (b',_) = h'' s and ((_,c'), s') = h s in ((b', c'), s')), s)  

let (>>>) (f : ('a,'s) co -> ('b, 's) co) (g : ('b,'s) co -> ('c, 's) co)  = 
  fun c -> g (f c) 

let loop (f : (('a * 'x), 's) co -> (('b * 'x), 's) co) : ('a, 'x * 's) co -> ('b, 'x * 's) co = 
  fun (Co (hi, (xi, si)) : ('a, 'x * 's) co) -> 
        Co (
            (fun (x, s) -> 
              let (Co (h', _s') : ('b * 'x, 's) co) = (* s' = s *)
                f (Co ((fun (s : 's) -> let (a,(b,c)) = hi (x,s) in ((a,b),c) ), s))            
              in let ((a, b),c) = h' s in 
                (a, (b, c))), 
            (xi, si))


(*  A PROUVER : les fonctions Yampa génére des sf qui ne modifient pas l'état !!! *)
(* leur implantation *)
(* The state is always the one of the global input *)

(* let first (f : ('a, 's1) co -> ('b, 's2) co) : ('a * 'c, 's1) co -> ('b * 'c, 's1 * 's2) co = 
  fun (Co (h, s))  ->  
    let (Co (h', s')) = f (Co ((fun s1 -> let (x,y) = h s1 in (fst x, y)), s)) in
      Co ((fun ((s1, s2)) -> 
        let (x, y) = h' s2 and (z,t) = h s1 in 
          ((x, snd z), (t,y))), (s,s'))   *)
        
(* composition *)

(* let compose (g : ('b,'s3) co -> ('c,'s4) co) (f : ('a, 's1) co -> ('b, 's2) co) : 
  ('a, ('s1 * 's3)) co -> ('c, ('s2 * 's4)) co =
        fun (Co (t, (s1,s3))) ->
          let (x, (y,z)) = t (s1,s3) in
          let (Co (h, i) : ('b, 's2) co) = f (Co ((fun s1 -> let (x,(y, _)) = t  (s1, s3) in (x,y)), s1)) in 
          let (Co (h', i') : ('c, 's3) co) = g (Co ((fun s3 -> let (x,(_,y)) = t (s1, s3) in (x,y)), s3)) in *)
          (* Co (f, ) *)
(* loop *)

(* possibilité de combiner une fois qu'on est sur les streams 
  mais pas sur les costreams ??? *)
(* let fst (f : ('a,'s1) co -> ('b, 's2) co) : ('a*'c,'s1) co -> ('a*'c,'s2) co = 
  fun (Co (g,s)) ->
    Co (fun s -> ) *)
  (* Co ((fun (a,s) -> ((a,?),s)), s) *)

(** For testing purpose*)

let from_list (l : 'a list) (a :'a) : ('a, 'a list) co =
  Co ((fun l -> match l with [] -> (a,l) | h::t -> (h, t)), l)

let rec to_list (Co (f,i) : ('a,'b) co) (n : int) = 
if n > 0 then 
  let (a, s') = f i in 
    a::(to_list (Co (f, s')) (n-1))
else []
  

  (* let rec apply_to_list (f : ('a,'b) t -> ('a,'c) t) (l : 'a list ) : 'a list =
  match l with 
    | [] -> []
    | a::t -> let (b,s') = f s in b::(apply_to_list (Co (f, s')) t) *)
