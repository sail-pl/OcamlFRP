open Coiterator
(* open Coiterator_examples *)

let map (f : 'a -> 'b) : (('a, 'c) co -> ('b, 'c) co) = 
  fun (Co (g, s)) -> 
    Co ((fun s -> let (b, g') = g s in (f b, g')), s)

let arr : ('a -> 'b) -> ('a, 'c) co -> ('b, 'c) co = map

let first (f : ('a, 's) co -> ('b, 's) co) : ('a * 'c, 's) co -> ('b * 'c, 's) co = 
  fun (Co (h, s) : ('a * 'c, 's) co)  ->  
    let h' : ('a, 's) co = Co ((fun s -> let ((x,_),y) = h s in (x, y)), s) in 
    let (Co (h'', _) : ('b, 's) co) = f h' in
      Co ((fun s -> let (b',_) = h'' s and ((_,c'), s') = h s in ((b', c'), s')), s)  

let (>>>) (f : ('a,'s) co -> ('b, 's) co) (g : ('b,'s) co -> ('c, 's) co)  = 
  fun c -> g (f c) 

let loop (f : (('a * 'x), 's) co -> (('b * 'x), 's) co) (_x : 'x): ('a, 'x * 's) co -> ('b, 'x * 's) co = 
  fun (Co (hi, (xi, si)) : ('a, 'x * 's) co) -> 
        Co (
            (fun (x, s) -> 
              let (Co (h', _s') : ('b * 'x, 's) co) = (* s' = s *)
                f (Co ((fun (s : 's) -> let (a,(b,c)) = hi (x,s) in ((a,b),c) ), s))            
              in let ((a, b),c) = h' s in (a, (b, c))), 
            (xi, si))

(* let arr (f : 'a -> 'b) :  ('a, 'c) co -> ('a, 'c) co = map f *)

(* f must be polymorphic to apply to each possible state *)
