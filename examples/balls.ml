open Graphics
open Ocamlfrp.Utils
open Ocamlfrp.Arrows
open Ocamlfrp.Mlgraphics
    
type pos = int * int

type direction = Left | Right | Up | Down | NoMove

let move (d : direction) (p : pos) : pos = 
  match (d, p) with 
    | Left, (x,y) -> (x - 1, y)
    | Right, (x,y) -> (x + 1, y)
    | Up, (x,y) -> (x, y + 1)
    | Down, (x,y) -> (x, y - 1)   
    | NoMove, (x,y) -> (x, y)

let mkball : ('a -> pos -> pos) -> pos ->  ('a , pos) sf = 
  fun f ->
    loop (arr (fun (a, p) -> dup (f a p)))

let ball1 : (direction, pos) sf =
  mkball move (340, 120)
  

let get_direction : (input, direction) sf =
  arr (fun s -> 
        match s.key with 
          | Some 'q' -> Left
          | Some 'd' -> Right
          | Some 'z' -> Up
          | Some 's' -> Down
          | _ -> NoMove)

let draw_circle : (pos, output) sf =
  arr (fun (x,y) -> 
    [fun () -> set_color black; fill_circle x y 10])
    
let showball : (input, output) sf = 
  get_direction >>> ball1 >>> draw_circle

let () = run showball (Some 0.0001)

(* avoir une boite qui permet d'avoir acces a status *)

(* let showmouse : (input, output) sf = 
  arr (fun s -> s.mouse_x,s.mouse_y) >>> 
    (arr (fun (x,y) -> 
      [fun () -> set_color black; fill_circle x y 10]))  
    
let () = run showmouse (Some 0.0001)*)
