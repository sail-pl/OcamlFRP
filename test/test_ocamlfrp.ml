open Format
open Fmt
open Ocamlfrp.Stream
open Ocamlfrp.Arrows

open Ocamlfrp.References

(* STREAMS *)

(** stream of () *)

let dummy : unit stream = constant ()

(** stream of positive integers *)

let positives : int stream = coiterate ((+) 1) 0

(* STREAM FUNCTIONS WITHOUT LOOPS *)

let identity : ('a,'a) sf = arr Fun.id

let plus_left : (int, int * int) sf = 
  arr Ocamlfrp.Utils.dup >>> first (arr (( + ) 1)) 

let squares : (int,int) sf = 
  arr Ocamlfrp.Utils.dup >>> (arr (Ocamlfrp.Utils.uncurry ( * )))

(* STREAM FUNCTIONS WITH LOOPS *)

let counter : ('a, int) sf = 
  let open Ocamlfrp.Utils in 
    loop (arr (mapright (( + ) 1) << dup << snd)) 0
  
let pre : 'a -> ('a,'a) sf = 
  loop (arr Ocamlfrp.Utils.swap)

let sum : (int, int) sf = 
  let open Ocamlfrp.Utils in 
  loop (arr (dup << uncurry ( + ))) 0 
  
(* STREAM FUNCTIONS WITH REFERENCES *)

let counter_with_ref  : ('a, int) sf = 
  let open Ocamlfrp.Utils in 
  let r = mkref 0 in 
    get r >>> arr ((mapright (( + ) 1)) << dup << snd) >>> set r

let pref_with_ref : 'a -> ('a,'a) sf = 
  fun v -> 
    let r = mkref v in get r >>> (arr Ocamlfrp.Utils.swap) >>> set r
    
let sum_with_ref : (int, int) sf =
  let open Ocamlfrp.Utils in 
  let r = mkref 0 in 
    get r >>> arr (dup << uncurry (+)) >>> set r

let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "positives:") (list_of_stream (lift identity positives) 10) 
let _ = Ocamlfrp.Utils.show (pair pp_print_int pp_print_int) (Some "plust_left:")  (list_of_stream (lift plus_left positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "squares positives:") (list_of_stream (lift squares positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "counter:") (list_of_stream (lift counter dummy) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "pre positives:") (list_of_stream (lift (pre 0) positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "sum positives:")(list_of_stream (lift sum positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "counter with ref:")(list_of_stream (lift counter_with_ref dummy) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "pre with ref:")(list_of_stream (lift (pref_with_ref 0) positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "sum with ref:")(list_of_stream (lift sum_with_ref positives) 10)
let _ = Format.fprintf std_formatter "done.\n" 