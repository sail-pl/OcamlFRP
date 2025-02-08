(*************************************************************************)
(*                                                                       *)
(*                                OCamlFRP                               *)
(*                                                                       *)
(* Copyright (C) 2025  Frédéric Dabrowski                                *)
(* All rights reserved.  This file is distributed under the terms of      *)
(* the GNU Lesser General Public License version 3.                      *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(*************************************************************************)

open Graphics

open Arrows
open FrpEngine

type renderObject = {draw : unit -> unit }

let mk_renderObject f = {draw = f}

type scene = renderObject list

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
