open Coiterators
open Utils

let value : ('a,'b) co -> 'a = fun (Co (h,s)) -> fst (h s)
let nextstate : ('a,'b) co -> 'b = fun (Co (h,s)) -> snd (h s)

let permutright : ('a * 'b) * 'c -> 'a * ('b * 'c) =
  fun ((a,b),c) -> (a, (b,c))

let permutleft : 'a * ('b * 'c) -> ('a * 'b) * 'c =
  fun (a,(b,c)) -> ((a,b),c)

(** split *)
let split : 'a 'b 'c. ('a * 'b, 's) co -> ('a, 's) co * ('b, 's) co =
  fun (Co (h,s)) ->
    Co ((fun s -> let ((a,_), s') = h s in (a, s')), s),
    Co ((fun s -> let ((_,c), s') = h s in (c, s')), s)

let join : 'a 'b 's1 's2. ('a, 's1 * 's2) co -> ('b, 's1) co -> ('a * 'b, 's1 * 's2) co =
  fun (Co (h1,(s1, s2))) (Co (h2, _)) ->
  Co ((fun (s1,s2) -> 
        let (a, (s1',s2')) = h1 (s1,s2) and (b, _) = h2 s1 in 
          (a,b), (s1',s2')
      ), (s1,s2))

let aux1 (Co (h,s) : ('a, 's) co) : 'x -> ('a * 'x, 's) co =
  fun x -> Co ((fun s -> let (a, s') = h s in ((a, x), s')), s) 

let aux2 (Co (h, (s1,s2) ) : ('a * 'x, ('s1 * 's2)) co) : 'x -> ('a, ('s1 *('s2 * 'x))) co =
  fun x0 -> Co ((fun (s1, (s2, _)) -> 
    let ((a,x'), (s1', s2')) = h (s1,s2) in (a, (s1', (s2',x')))), (s1, (s2,x0)))
        

(********************************************************)

module Raw = 
    struct 

let arr : 'a 'b 's. ('a -> 'b) -> ('a, 's) co -> ('b, ('s * unit)) co = 
  fun f (Co (h, s)) -> 
    Co ((fun (s,()) -> let (a,s') = h s in (f a, (s',()))), 
    (s,()))

(** The stream function [arr f] applies the function [f] to each element of a stream.
    It satisfies the following equations :
   - head ((arr f) s) = f (head s) 
   - tail ((arr f) s) = arr f (tail s) *)

(** The unary [first] operator applies a stream function to the left members of a stream of pairs.*)

let first : 'a  'b 'c 's1 's2. 
  (('a, 's1) co -> ('b, 's1 *'s2) co) -> 
    ('a * 'c, 's1) co -> ('b * 'c, 's1 * 's2) co =
  fun f (Co (h, s))  ->
    let Co (h1, (s1,s2)) = f (Co ((mapleft fst << h), s)) in
      Co (
          (fun (s1,s2) -> 
            let (a, (s1',s2')) = h1 (s1,s2) 
            and b = fst ((mapleft snd << h) s1) in 
              (a,b), (s1',s2')
          ), (s1,s2))
          
(** The binary composition operator (>>>) composes two operators.*)
let (>>>) : 'a 'b 'c 's1 's2 's3.
  (('a,'s1) co -> ('b, 's1 * 's2) co) -> 
    (('b,'s1 * 's2) co -> ('c, ('s1 * 's2) * 's3) co) -> 
      (('a,'s1) co -> ('c, 's1 * ('s2 * 's3)) co) = 
  fun f g c -> 
    let Co (h3, s) = g (f c) in
      Co ((mapright permutright) << h3 << permutleft, permutright s)
      

(** The unary [loop] operator plugs a stream function to a register *)            

let loop : 'a 'b 'x 's1 's2. ((('a * 'x), 's1) co -> (('b * 'x), 's1 * 's2) co) -> 
    'x -> ('a, 's1) co -> ('b, 's1 * ('s2 * 'x)) co = 
  fun f x0 -> 
    fun (Co (h, s1) : ('a, 's1) co) -> 
      let g = fun x -> aux2 (f (aux1 (Co (h, s1)) x)) x in 
    Co ((fun (s1, (s2,x)) -> 
      let Co (h',_) = g x in h' (s1,(s2,x))), 
      let Co (_,s) = g x0 in s)

    end

type 'a stream = Str : ('a, 's) co -> 'a stream

(* rename fx in sf *)
(* use type stream ? *)

(* sf does not work on streams because we need s1 -> s * s2 *)
type ('a,'b) sf = 
    SF : {fx : 's. (('a, 's) co -> ('b, 's * 's2) co)} ->('a,'b) sf

let arr : 'a 'b. ('a -> 'b) -> ('a,'b) sf =
  fun f ->
    SF {fx = 
          (fun (Co (h, s)) ->
            Co ((fun (s,()) -> let (a,s') = h s in (f a, (s',()))), 
          (s,())))}

let first : 'a  'b 'c. ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf =
  fun (SF {fx=f}) -> 
    SF {fx = 
    fun (Co (h, s))  ->
      let Co (h1, (s1,s2)) = f (Co ((mapleft fst << h), s)) in
        Co (
            (fun (s1,s2) -> 
              let (a, (s1',s2')) = h1 (s1,s2) 
              and b = fst ((mapleft snd << h) s1) in 
                (a,b), (s1',s2')
            ), (s1,s2))
    }

let (>>>) : ('a, 'b) sf -> ('b,'c) sf -> ('a, 'c) sf =
  fun (SF {fx = f})
    (SF {fx = g}) ->
      SF {fx = 
        fun c -> 
          let Co (h3, s) = g (f c) in
          Co ((mapright permutright) << h3 << permutleft, permutright s)}
      
let loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf =
  fun (SF {fx = f}) x0 -> 
    SF {fx = 
      fun (Co (h, s1)) -> 
        let g = fun x -> aux2 (f (aux1 (Co (h, s1)) x)) x in 
          Co ((fun (s1, (s2,x)) -> 
              let Co (h',_) = g x in h' (s1,(s2,x))), 
              let Co (_,s) = g x0 in s)
      }
                
(** Derived operators *)

let constant a = arr (const a)

let second f = arr swap >>> first f 

let parallel f g = first f >>> second g 

let fork f g = arr dup >>> parallel f g


