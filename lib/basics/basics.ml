open Coiterators
(* open Arrows *)

(* let identity : ('a,'s) co -> ('a, 's * unit) co = arr (fun x-> x) *)

let constant : 'a -> ('a,'s) co = 
  fun c -> Co ((fun () -> (c,())), ())

