(******************************************************************************)
(*                                                                            *)
(*                                  OCamlFRP                                  *)
(*                                                                            *)
(* Copyright (C) 2025 Frédéric Dabrowski                                      *)
(* Copyright (C) 2025 Nicolas Paul                                            *)
(* All rights reserved.  This file is distributed under the terms of          *)
(* the GNU Lesser General Public License version 3.                           *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.     *)
(******************************************************************************)

open Arrows

(** [Environment] is an execution environment in which runs and is consumed
    a reactive program (an arrow most likely). *)
module type Environment = 
  sig 
    (** [appinput] is the type of value fed into a reactive program in the
        environment. *)
    type appinput

    (** [appoutput] is the type of value produced by a reactive program in the
        environment. *)
    type appoutput

    (** [init ()] initialises the environment for a reactive program, e.g.
         setting up the window for drawing. *)
    val init : unit -> unit

    (** [stop ()] stops the execution of a reactive program. *) 
    val stop : unit -> unit 

    (** [input b] is the stream of input values for a reactive engine. *)
    val input : bool -> appinput 
    (* TODO(nico): what is the use of the boolean input? *)

    (** [output o] is a function which consumes the output [o] of a reactive
        program as long as it returns [true]. *)
    val output : appoutput -> bool  
  end 

(** [Engine] is a mono-clocked execution system for reactive programs in a
    specific environment. *)
module Engine : functor (E : Environment) -> 
  sig 
    (** [run f d] initialises the environment and starts executing synchronous
        function [f] as its reactive program, consuming its values with
        an optional delay [d] between each consumption. *)
    val run : (E.appinput, E.appoutput) sf -> float option -> unit
  end
