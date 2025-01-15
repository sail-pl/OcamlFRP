open Graphics

open Arrows
open Animate

type input = Graphics.status 
type output = (unit -> unit) list

module Graph : Environment with 
  type appinput = input 
  and type appoutput = output= 
  struct 
  
  type appinput = input

  type appoutput =  output

  let init () = open_graph " 640x480"

  let stop () = ()

  let input _ = 
    wait_next_event 
      [Button_down; Button_up; Key_pressed; Mouse_motion]

  let output (o : (unit -> unit) list) = 
    try 
      clear_graph () ;
      List.iter (fun f -> f ()) o;
      true
    with _ -> print_string "Error"; false
end

module E = Engine (Graph)

let run : (input, output) sf -> float option -> unit = E.run
