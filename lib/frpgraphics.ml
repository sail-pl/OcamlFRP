open Graphics

open Arrows
open Animate

(** Add size_x, size_y to status *)
(* make values for outputs *)

type pos = {
  x : int;
  y : int
}

type renderObject = {draw : unit -> unit }

let mk_renderObject f = {draw = f}

type scene = renderObject list

(* let circle : ((pos * int * color), renderObject) sf = 
  let f = 
    fun (center, radius, c) -> 
      {
        p = center;
        draw = fun () -> 
          set_color c; 
          fill_circle center.x center.y radius
      }
  in arr f *)

type input = {
  window_size_x : int;
  window_size_y : int;
  mouse_pos_x : int;
  mouse_pos_y : int;
  mouse_button : bool;
  key : char option;
}

type output = scene


let fetch () = 
  let s = wait_next_event 
    [Button_down; Button_up; Key_pressed; Mouse_motion;Poll] in
  {
    window_size_x = size_x ();
    window_size_y = size_y ();
    mouse_pos_x = s.mouse_x;
    mouse_pos_y = s.mouse_y;
    mouse_button = s.button;
    key = if s.keypressed then Some (read_key ()) else None
  }

module Graph : Environment with 
  type appinput = input 
  and type appoutput = scene= 
  struct 
  
  type appinput = input

  type appoutput =  scene

  let init () = 
    open_graph " 640x480"

  let stop () = ()

(* add a case to wait for inputs *)
(* add a wait to specify input type ? *)

  let input _b = fetch ()

  let output (o : scene) = 
    try 
      clear_graph () ;
      List.iter (fun f -> f.draw ()) o;
      true
    with _ -> print_string "Error"; false
end

module E = Engine (Graph)

let run : (input, output) sf -> float option -> unit = E.run
