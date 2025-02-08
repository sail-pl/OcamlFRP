open Arrows

(* let identity : ('a,'a) sf = 
  arr (fun x -> x) *)

let const : 'a -> ('b,'a) sf = 
  fun a -> arr (fun _ -> a)

let delay : 'a -> ('a, 'a) sf = 
  fun a -> loop (arr Utils.swap) a
