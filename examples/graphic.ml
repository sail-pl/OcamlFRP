open Graphics
open Ocamlfrp.Arrows
open Ocamlfrp.Mlgraphics
        
let showmouse : (input, output) sf = 
  arr (fun s -> s.mouse_pos_x,s.mouse_pos_y) >>> 
    (arr (fun (x,y) -> 
      [fun () -> set_color black; fill_circle x y 10])) 
    
let () = run showmouse (Some 0.0001)
