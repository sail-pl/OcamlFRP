open Format
open Ocamlfrp.Util
open Ocamlfrp.Coiterator
open Ocamlfrp.Yampa

(* STREAMS *)

(* stream of () *)

let dummy : (unit, unit) co = Co (dup, ())

(* stream of positive integers *)

let positives : (int, int) co = Co ((mapright ((+) 1) << dup), 0) 

(* STREAM FUNCTIONS WITHOUT LOOPS *)

let squares = arr dup >>> (arr (uncurry ( * )))

(* STREAM FUNCTIONS WITH LOOPS *)

let counter = 
  let f = fun ((), n) -> (n, n+1) in
  loop (arr f) 0

let pre = loop (arr swap)

let sum = 
  let f = (fun (x,y) -> dup (x + y)) in 
  loop (arr f) 0

(* References *)

type 'a cell = {content : 'a ref}

let mkref x = {content = ref x}

let get : 'r cell -> ('a, 'x) co -> ('r * 'a, 'x) co =
  fun r (Co (h,x)) -> 
    Co ((fun x -> let (a,x') = h x in ((!(r.content), a), x')) , x)

let set : 'r cell -> ('r * 'a, 'x) co -> ('a, 'x) co = 
  fun r (Co (h, x)) -> 
    Co ((fun x -> let ((v,a), x') = h x in r.content := v; (a,x')), x)

let counter_with_ref = 
  let r = mkref 0 in 
    get r >>> arr (fun (x, ()) -> (x+1, x)) >>> set r
    
let _ = show (pp_print_int) (Some "positives:") (to_list positives 10)
let _ = show (pp_print_int) (Some "squares positives:") (to_list (squares positives) 10)
let _ = show (pp_print_int) (Some "counter:") (to_list (counter dummy) 10)
let _ = show (pp_print_int) (Some "pre positives:") (to_list (pre 0 positives) 10)
let _ = show (pp_print_int) (Some "sum positives:")(to_list (sum positives) 10)
let _ = show (pp_print_int) (Some "counter with ref:")(to_list (counter_with_ref dummy) 10)
let _ = Format.fprintf std_formatter "done.\n"
    
