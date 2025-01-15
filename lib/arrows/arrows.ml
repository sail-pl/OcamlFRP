open Coiterators
open Utils
        
type 'a stream = Str : ('a, 's) co -> 'a stream

let coiterate : 'a 's. ('s -> 'a * 's) -> 's -> 'a stream = 
  fun h s -> Str (Co (h,s)) 

let fold : 'a. ('a -> 'a) -> 'a -> 'a stream = 
  fun f x0 -> coiterate (fun x -> let y = f x in (x,y)) x0

let constant : 'a. 'a -> 'a stream =
  fun x -> fold (Fun.const x) x

let list_of_stream : 'a stream -> int -> 'a list =
  fun (Str s : 'a stream) (n : int) : 'a list ->
    to_list s n

let rec perform : 'a stream -> ('a -> unit) -> int -> unit = 
  fun s f n ->
    if n <= 0 then ()
    else 
      let (Str ((Co (h,s)))) =  s in 
      let (a,s') = h s in f a; 
        perform (Str ((Co (h,s')))) f (n-1)

let rec tperform : 'a stream -> ('a -> bool) -> float option -> unit = 
  fun s f d ->
    let (Str ((Co (h,s)))) =  s in 
    let (a,s') = h s in 
    let b = f a in
      (* print_string "a\n"; flush stdout; *)
      match d with None -> () | Some t -> Thread.delay t;
      if b then 
        tperform (Str ((Co (h,s')))) f d
      else ()      

type ('a,'b) sf = 
    SF : {fx : 's. (('a, 's) co -> ('b, 's * 's2) co)} ->('a,'b) sf

(** [apply sf s] applies the stream function sf to the stream s.*)
let apply : ('a,'b) sf -> 'a stream -> 'b stream = 
  fun (SF { fx = f}) (Str c) -> Str (f c)
    
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
          Co ((mapright Utils.permutright) << h3 << Utils.permutleft, Utils.permutright s)}
      
let aux1 (Co (h,s) : ('a, 's) co) : 'x -> ('a * 'x, 's) co =
  fun x -> Co ((fun s -> let (a, s') = h s in ((a, x), s')), s) 

let aux2 (Co (h, (s1,s2) ) : ('a * 'x, ('s1 * 's2)) co) : 'x -> ('a, ('s1 *('s2 * 'x))) co =
  fun x0 -> Co ((fun (s1, (s2, _)) -> 
    let ((a,x'), (s1', s2')) = h (s1,s2) in (a, (s1', (s2',x')))), (s1, (s2,x0)))
          
let loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf =
  fun (SF {fx = f}) x0 -> 
    SF {fx = 
      fun (Co (h, s1)) -> 
        let g = fun x -> aux2 (f (aux1 (Co (h, s1)) x)) x in 
          Co ((fun (s1, (s2,x)) -> 
              let Co (h',_) = g x in h' (s1,(s2,x))), 
              let Co (_,s) = g x0 in s)
      }

let second  : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf = 
  fun f -> arr swap >>> first f >>> arr swap

let parallel : ('a,'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf = 
  fun f g -> first f >>> second g 

let fork : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf  = 
  fun f g -> arr dup >>> parallel f g
