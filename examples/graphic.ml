open Graphics
open Ocamlfrp.Arrows
open Ocamlfrp.Mlgraphics

(** Programme utilisant sf *)

let mousepos : (input, int * int) sf = 
    arr (fun s -> s.mouse_x,s.mouse_y)
    
let showmouse : (input, output) sf = 
  mousepos >>> 
    (arr (fun (x,y) -> [fun () -> set_color black; fill_circle x y 20])) 
    
let () = run showmouse (Some 0.0001)

(* TODO *)
(* REFAIRE L'exemple de main avec engine *)
(* AJOUTER DES SF pour combiner les objets *)
(* PERMETTRE DE QUITTER (* OBJET *) *)
(* COMBINER DES MODULES ENVIRONEMENT *)
