open Graphics
open Unix 
open Format 

open Ocamlfrp.Animate
open Ocamlfrp.Arrows
open Ocamlfrp.References

(*******************************************************)
(** General form of the environment for executing a sf *)
(*******************************************************)

module type Environment = 
  sig 
    (** type of input *)
    type appinput 

    (** type of output *)
    type appoutput
    (** init function *)
    val init : unit -> unit
    val stop : unit -> unit
    val input : unit -> appinput 
    val output : appoutput -> unit  
  end 

(* runtime for executing a sf *)
module Engine (E : Environment)= 
  struct
    let run (f : ('a,'b) sf) (d : float option) = 
        E.init ();
        let s = coiterate (fun () -> (E.input (),())) ()  in 
          tperform (apply f s) E.output d
  end

(* Partie module graphique *)

(* elements accessibles à l'utilisateur *)

(* graphics status : give another name *)

type obj = 
  | Circle of int * int * int * color
    
let mkcircle (x, y, r,c) = 
  Circle (x,y,r,c)


module Graph : Environment with 
  type appinput = Graphics.status 
  and type appoutput = obj list = 
  struct 

    type appinput = Graphics.status

    type appoutput =  obj list 

    let init () = open_graph " 640x480"

    let stop () = ()

    let input () = 
      wait_next_event 
        [Button_down; Button_up; Key_pressed; Mouse_motion]

    let output (o : obj list) = 
      let renderobj o = 
        match o with 
          Circle (x,y,r,c) -> 
            set_color c; 
            fill_circle x y r
          in 
      try 
        clear_graph () ;
        List.iter renderobj o
      with _ -> print_string "Error"
  end

module E = Engine (Graph)

let run : (Graphics.status, obj list) sf -> float option -> unit = E.run

(** Programme utilisant sf *)

let mousepos : (Graphics.status, int * int) sf = 
    arr (fun s -> s.mouse_x,s.mouse_y)
    
let showmouse : (Graphics.status, obj list) sf = 
  mousepos >>> 
    (arr (fun (x,y) -> [mkcircle (x,y,20,black)])) 
    
let () = run showmouse (Some 0.0001)

(* TODO *)
(* MOVE ENVIRONMENT AND ENGINE TO A NEW LIB (ANIMATE ??) *)
(* MOVE TO A NEW LIBRARY GRAPHICS *)
(* REFAIRE L'exemple de main avec engine *)

