open Format
open Fmt

open Ocamlfrp.Utils
open Ocamlfrp.Coiterators
open Ocamlfrp.Arrows
open Ocamlfrp.References

(* STREAMS *)

(* stream of () *)

let dummy : (unit, unit) co = Co (dup, ())

  (* stream of positive integers *)

let positives : (int, int) co = Co ((mapright ((+) 1) << dup), 0) 

(* STREAM FUNCTIONS WITHOUT LOOPS *)

let identity : ('a,'s) sf = SF (arr (fun x-> x))


let identity : ('a,'s) co -> ('a, 's * unit) co = arr (fun x-> x)

let plus_left = arr dup >>> first (arr (( + ) 1))

let squares = arr dup >>> (arr (uncurry ( * )))

(* STREAM FUNCTIONS WITH LOOPS *)

let counter = loop (arr (mapright (( + ) 1) << dup << snd)) 0
  
let pre = loop (arr swap)

let sum = loop (arr (dup << uncurry ( + ))) 0
  
(* STREAM FUNCTIONS WITH REFERENCES *)

let counter_with_ref : (unit, unit) co -> (int, unit * ((unit * unit) * unit)) co = 
  let r = mkref 0 in 
    get r >>> arr ((mapleft (( + ) 1)) << dup << snd) >>> set r
    
let pref_with_ref v = 
  let r = mkref v in get r >>> set r

let sum_with_ref =
  let r = mkref 0 in 
    get r >>> arr (dup << uncurry (+)) >>> set r

let _ = show (pp_print_int) (Some "positives:") (to_list positives 10)
let _ = show (pair pp_print_int pp_print_int) (Some "plust_left:") (to_list (plus_left positives) 10)
let _ = show (pp_print_int) (Some "squares positives:") (to_list (squares positives) 10)
let _ = show (pp_print_int) (Some "counter:") (to_list (counter dummy) 10)
let _ = show (pp_print_int) (Some "pre positives:") (to_list (pre 0 positives) 10)
let _ = show (pp_print_int) (Some "sum positives:")(to_list (sum positives) 10)
let _ = show (pp_print_int) (Some "counter with ref:")(to_list (counter_with_ref dummy) 10)
let _ = show (pp_print_int) (Some "pre with ref:")(to_list (pref_with_ref 0 positives) 10)
let _ = show (pp_print_int) (Some "sum with ref:")(to_list (sum_with_ref positives) 10)
let _ = Format.fprintf std_formatter "done.\n"
    

(* let get : 'r cell -> ('a, 'x) co -> ('r * 'a, 'x) co =
  fun r (Co (h,x)) -> 
    Co ((fun x -> let (a,x') = h x in ((!(r.content), a), x')) , x) *)

    (* let set : 'r cell -> ('r * 'a, 'x) co -> ('a, 'x) co = 
      fun r (Co (h, x)) -> 
        Co ((fun x -> let ((v,a), x') = h x in r.content := v; (a,x')), x) *)
