open Ocamlfrp.Arrows
open Ocamlfrp.References

let uncurry f (x,y) = f x y
let dup = fun x -> (x,x)

let _ = init ()

let _ = 
  let read_random = input_ref (fun () -> Random.int 100000) in
  let print_line = output_ref (fun s -> print_endline s; flush stdout) in
  let maxsf  = 
      get_unit read_random 
      >>> loop 0
            ( arr (uncurry max) 
              >>> arr dup 
              >>> first (arr (string_of_int)) )
      >>> set_unit print_line
    in run maxsf
