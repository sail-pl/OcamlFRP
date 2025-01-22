open Coiterators
open Utils
open Stream   

(* length preserving synchronous functions can be written using an iterator *)
(* SF are length preserving functions, we define them as iterators *)
(* We use the existential types of GADT to hide the state *)

(* SF A B = A -> (B, SF A B) *)

(* pour les sf générés par les arrows on a une traduction vers nos sf *)
(* celle ci est donnée ci dessous *)

(* module type functor *)
(* module type monad *)

(* Functor *)

type ('s, 'a) st = 
  St : ('s -> ('a * 's)) -> ('s, 'a) st

let map : ('a -> 'b) -> ('s, 'a) st -> ('s, 'b) st =
    fun f (St t) ->
    St (fun s -> let (a,s') = t s in (f a, s'))
  
(* Monad *)

let return : 'a -> ('s, 'a) st =
  fun a -> St (fun s -> (a, s))
  
let ( >>= ) : ('s, 'a) st -> ('a -> ('s, 'b) st) -> ('s, 'b) st = 
  fun (St t) f -> 
    St (fun s -> 
          let (a, s') = t s in 
            let St t' = f a in t' s')

type ('a, 'b) sf = 
  SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

let arr : ('a -> 'b) -> ('a, 'b) sf = 
  fun f -> SF ((fun () a -> (f a, ())), ())

let first : ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf =
  fun (SF (f, s)) ->
    SF ((fun s (a, c) -> let (b, s') = f s a in ((b, c), s')), s)

let (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf =
  fun (SF (f, s1)) (SF (g, s2)) ->
    SF ((fun (s1, s2) a -> let (b, s1') = f s1 a in let (c, s2') = g s2 b in (c,(s1', s2'))), (s1, s2))
  
let loop : ('a * 'c, 'b * 'c) sf -> 'c -> ('a, 'b) sf =
  fun (SF (f,s)) (c : 'c) ->
    SF ((fun (s, c) a -> let ((b, c'), s') = f s (a, c) in (b, (s', c'))), (s, c))

(* now we can lift sf to stream functions *)

(* type 'a stream = Str  *)

type 'a stream = Str : ('s, 'a) st * 's -> 'a stream

let lift : ('a, 'b) sf -> 'a stream -> 'b stream =
fun (SF (f,s)) -> 
      fun (Str (St h, s1)) -> 
        Str ( St
          (fun (s1, s) -> 
            let (a, s1') = h s1 in 
              let (b, s') = f s a in 
                (b, (s1', s')))
          ,
            (s1, s))

(* derived operators *)

let second  : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf = 
  fun f -> arr swap >>> first f >>> arr swap

let parallel : ('a,'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf = 
  fun f g -> first f >>> second g 

let fork : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf  = 
  fun f g -> arr dup >>> parallel f g

(* let sf_of_stream : 'a stream -> ('b,'a) sf = 
  fun s -> loop (arr (fun (_, s) -> (head s, tail s)))  s *)
  
(* Can we have operators directly defined on streams. Not efficient *)
(* type ('a,'b) co_fun =  *)
    (* CF : {fx : 's. (('a, 's) co -> ('b, 's * 's2) co)} ->('a,'b) co_fun *)

    module M = 
struct

(* Functor *)

type ('s, 'a) st = 
  St : ('s -> ('a * 's)) * 's -> ('s, 'a) st

let map : ('a -> 'b) -> ('s, 'a) st -> ('s, 'b) st =
    fun f (St (t,s)) ->
    St ((fun s -> let (a,s') = t s in (f a, s')), s)
  
(* Monad *)
(* structure de monoid sur les états *)

let return : 's -> 'a -> ('s, 'a) st =
  fun s a -> St ((fun s -> (a, s)), s)
  
let ( >>= ) : ('s -> 's -> 's) -> ('s, 'a) st -> ('a -> ('s, 'b) st) -> ('s, 'b) st = 
  fun op (St (t,s)) f -> 
    St ((fun s -> 
          let (a, s') = t s in 
            let St (t', s'') = f a in 
            let (b,s''') = t' s' in (b, op s'' s''')), s)

    end