open Coiterators

(* streams must not perform side effects *)

type 'a stream = Str : ('a, 's) co -> 'a stream

let head : 'a stream -> 'a =
  fun (Str (Co (h,s))) -> 
    fst (h s)  

let tail : 'a stream -> 'a stream =
  fun (Str (Co (h,s))) -> 
    Str (Co (h, snd (h s)))
  
(* let stream_of_iterator (c : ('a,'s) co) : 'a stream = Str c
let iterator_of_stream (Str c : 'a stream) :('a,'b) co  = c *)

(* map *)

(* let apply_in (f : ('a,'s) co -> ('b, 's2) co) (Str c : 'a stream) = 
  Str (f c) *)

let produce : 'a 's. ('s -> 'a * 's) -> 's -> 'a stream = 
  fun h s -> Str (Co (h,s)) 

let coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream = 
  fun f x0 -> produce (fun x -> let y = f x in (x,y)) x0

let constant : 'a. 'a -> 'a stream =
  fun x -> coiterate (Fun.const x) x

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

let rec consume : 'a stream -> ('a -> bool) -> float option -> unit = 
  fun s f d ->
    let (Str ((Co (h,s)))) =  s in 
    let (a,s') = h s in 
    let b = f a in
      (* print_string "a\n"; flush stdout; *)
      match d with None -> () | Some t -> Thread.delay t;
      if b then 
        consume (Str ((Co (h,s')))) f d
      else ()   