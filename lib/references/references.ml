(* open Coiterators *)
open Arrows

type 'a cell = {content : 'a ref}

let mkref x = {content = ref x}

let get : 'r cell ->  ('a, 'a * 'r) sf =
  fun r -> arr (fun a -> (a, !(r.content)))

let set : 'r cell -> ('a * 'r, 'a) sf =
  fun r -> arr (fun (a,x) -> (r.content := x) ;a)

