open Ocamlfrp.Utils
open Ocamlfrp.Arrows
open Ocamlfrp.Animate

module Std : Environment with type appinput = string and type appoutput = string = 
  struct 
    type appinput = string 
    type appoutput = string 
    let init _ = ()
    let stop _ = ()
    let input _ = read_line ()
    let output s = print_endline s; true
  end 

module E = Engine(Std)

let _ = 
  let f (x,y) = dup (y ^ x) in
    E.run (loop (arr f) "") (Some 0.001)
