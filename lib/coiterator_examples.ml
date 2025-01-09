open Coiterator

let _map (f : 'a -> 'b) : (('a, 'c) co -> ('b, 'c) co) = 
  fun (Co (g, s)) -> 
    Co ((fun s -> let (b, g') = g s in (f b, g')), s)

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