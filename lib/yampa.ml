open Coiterator
open Util

(** split *)
let split : 'a 'b 'c. ('a * 'b, 's) co -> ('a, 's) co * ('b, 's) co =
  fun (Co (h,s)) ->
    Co ((fun s -> let ((a,_), s') = h s in (a, s')), s),
    Co ((fun s -> let ((_,c), s') = h s in (c, s')), s)
  
let join : 'a 'b 's1 's2. ('a, 's1) co -> ('b, 's2) co -> ('a * 'b, 's1 * 's2) co =
  fun (Co (h1,s1)) (Co (h2,s2)) ->
  Co ((fun (s1,s2) -> 
        let (a,s1') = h1 s1 and (b, s2') = h2 s2 in 
          (a,b), (s1',s2')
      ), (s1,s2))

let aux1 (Co (h,s) : ('a, 's) co) : 'x -> ('a * 'x, 's) co =
  fun x -> Co ((fun s -> let (a, s') = h s in ((a, x), s')), s) 
      
let aux2 (Co (h, s) : ('a * 'x, 's) co) : 'x -> ('a, 's * 'x) co =
  fun x0 -> Co ((fun (s, _) -> let ((a,x'), s') = h s in (a, (s',x'))), (s, x0))
      

(********************************************************)

(** The stream function [arr f] applies the function [f] to each element of a stream.
    It satisfies the following equations :
   - head ((arr f) s) = f (head s) 
   - tail ((arr f) s) = arr f (tail s) *)
let arr : 'a 'b 'c. ('a -> 'b) -> ('a, 'c) co -> ('b, 'c) co = 
  fun f (Co (h, s)) -> Co (((mapleft f) << h), s)

(** The unary [first] operator applies a stream function to the left members of a stream of pairs.*)
  let first : 'a  'b 'c 's1 's2. 
  (('a, 's1) co -> ('b, 's2) co) -> ('a * 'c, 's1) co -> ('b * 'c, 's2 * 's1) co =
    fun f c ->
      let (c1, c2) = split c in join (f c1) c2

(** The binary composition operator (>>>) composes two operators.*)
let (>>>) : (('a,'s1) co -> ('b, 's2) co) -> (('b,'s2) co -> ('c, 's3) co) -> 
  (('a,'s1) co -> ('c, 's3) co) = 
  fun f g c -> g (f c)

(** The unary [loop] operator plugs a stream function to a register *)
let loop : ((('a * 'x), 's1) co -> (('b * 'x), 's2) co) -> 'x -> ('a, 's1) co -> ('b, 's2 * 'x) co = 
  fun f x0 -> fun (Co (h, s1) : ('a, 's1) co) -> 
    Co ( 
        (fun (s2, x) -> 
          let (Co (h', _)) = aux2 (f (aux1 (Co (h,s1)) x)) x 
            in h' (s2,x)),
          let (Co (_, s)) = aux2 (f (aux1 (Co (h,s1)) x0)) x0 
            in s)
            

(** Derived operators *)

let constant = fun a -> arr (const a)

let second : (('a,'s1) co -> ('b, 's2) co) -> ('c * 'a, 's1) co -> (('c * 'b), 's2 * 's1) co= 
  fun f-> (arr swap >>> first f) 

let parallel =
  fun f g -> first f >>> second g 

let fork = 
  fun f g -> arr dup >>> parallel f g
