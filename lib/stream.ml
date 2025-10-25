(*************************************************************************)
(*                                                                       *)
(*                                OCamlFRP                               *)
(*                                                                       *)
(* Copyright (C) 2025  Frédéric Dabrowski                                *)
(* All rights reserved.  This file is distributed under the terms of      *)
(* the GNU Lesser General Public License version 3.                      *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(*************************************************************************)

(* Streams are concrete streams in the sense of Caspi et Pouzet *)
(* We use GADT to encapsulate the internal state as an existential type *)
open Utils

type 'a stream = 
  | Str : ('state -> ('a * 'state)) * 'state -> 'a stream

let stream (f : 'c -> 'a *'c) (x : 'c) : 'a stream = Str (f, x)

let map : ('a -> 'b) -> 'a stream -> 'b stream = 
  fun f (Str (step, s )) -> Str ((fun s -> first f (step s)), s)

let coiterate : ('c -> 'a -> 'b * 'c) -> 'c -> 'a stream -> 'b stream =
  fun f x s ->
  let Str (g, sts) = s in 
    Str ((fun (st1, st2) -> 
            let (a, st1') = g st1 in 
            let (b, st2') = f st2 a in 
                (b, (st1',st2'))
            ),
    (sts, x))


let apply : 'a 'b. ('a -> 'b) stream -> 'a stream -> 'b stream =
  fun (Str (f, i)) (Str (e, ie)) ->
    Str ((fun (sf,se) -> 
      let (vf,sf') = f sf in
      let (ve,se') = e se in
        ((vf ve), (sf', se'))),
        (i,ie))

(* cas général de l'itérateur *)

(* let coiterate (x : 'c) (f : 'c -> 'a -> 'b * 'c) *)

(** Synchronous stream functions *)

type ('a, 'b) sf = 
  SF : ('state -> 'a -> 'b * 'state) * 'state -> ('a, 'b) sf

(** homomorphism *)

let lift (sf : ('a, 'b) sf) (s : 'a stream) : 'b stream =  
  let Str (g, sts) = s in 
  let SF (f, stf) = sf in
      Str ( 
            (fun (st1, st2) -> 
              let (a, st1') = g st1 in 
              let (b, st2') = f st2 a in 
                (b, (st1',st2'))
            ), 
            (sts,stf))

(* Category *)

let id : ('a, 'a) sf = 
  SF ((fun () a -> (a, ())), ())

let compose (SF (t1, s1) : ('a, 'b) sf) (SF (t2, s2) : ('b, 'c) sf) : ('a, 'c) sf =
      SF ((fun (s1, s2) a -> 
            let (b, s1') = t1 s1 a in 
              let (c, s2') = t2 s2 b in 
                (c, (s1', s2'))),
              (s1,s2))

(* HUM *)

let destr : 'a stream -> 'a * 'a stream = 
    fun (Str (f,s)) ->
      let (a,s') = f s in (a, Str (f,s'))

let head : 'a stream -> 'a =
  fun s -> fst (destr s) 

let tail : 'a stream -> 'a stream =
  fun s -> snd (destr s)
    

let produce : 'a 's. ('s -> 'a * 's) -> 's -> 'a stream = 
  fun h s -> Str (h,s) 

(* let coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream = 
  fun f x0 -> produce (fun x -> let y = f x in (x,y)) x0 *)


  
let rec perform : 'a stream -> ('a -> unit) -> int -> unit = 
  fun s f n ->
    if n <= 0 then ()
    else 
      let (Str (h,s)) =  s in 
      let (a,s') = h s in f a; 
        perform (Str (h,s')) f (n-1)

let rec consume : 'a stream -> ('a -> bool) -> float option -> unit = 
  fun s f d ->
    let (Str (h,s)) =  s in 
    let (a,s') = h s in 
    let b = f a in
      (* print_string "a\n"; flush stdout; *)
      match d with None -> () | Some t -> Thread.delay t;
      if b then 
        consume (Str (h,s')) f d
      else ()   

let stream_of_list (l : 'a list) (a :'a) : 'a stream =
  Str ((fun l -> match l with [] -> (a,l) | h::t -> (h, t)), l)
      
let rec list_of_stream (Str (f,i) : 'a stream) (n : int) = 
  if n > 0 then 
    let (a, s') = f i in 
      a::(list_of_stream (Str (f, s')) (n-1))
  else []
