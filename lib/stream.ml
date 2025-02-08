
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

type 'a stream = 
  | Str : ('state -> ('a * 'state)) * 'state -> 'a stream

let destr : 'a stream -> 'a * 'a stream = 
    fun (Str (f,s)) ->
      let (a,s') = f s in (a, Str (f,s'))

let head : 'a stream -> 'a =
  fun s -> fst (destr s) 

let tail : 'a stream -> 'a stream =
  fun s -> snd (destr s)
  
let map : 'a 'b. ('a -> 'b) -> 'a stream -> 'b stream = 
  fun f (Str (h,s)) -> 
    Str ((fun s -> let (a,s') = h s in (f a, s')), s)

let apply : 'a 'b. ('a -> 'b) stream -> 'a stream -> 'b stream =
  fun (Str (f, i)) (Str (e, ie)) ->
    Str ((fun (sf,se) -> 
      let (vf,sf') = f sf in
      let (ve,se') = e se in
        ((vf ve), (sf', se'))),
        (i,ie))
  

let produce : 'a 's. ('s -> 'a * 's) -> 's -> 'a stream = 
  fun h s -> Str (h,s) 

let coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream = 
  fun f x0 -> produce (fun x -> let y = f x in (x,y)) x0

let constant : 'a. 'a -> 'a stream =
  fun x -> coiterate (Fun.const x) x
  
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