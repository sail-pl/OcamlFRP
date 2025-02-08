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

open Arrows

module type Environment = 
  sig 
    (* type of input *)
    type appinput 
    (* type of output *)
    type appoutput
    (* init function *)
    val init : unit -> unit
    val stop : unit -> unit 
    (* used to build the input stream *)
    val input : bool -> appinput 
    (* used to consume the ouput stream*)
    val output : appoutput -> bool  
  end 

module Engine : functor (E : Environment) -> 
  sig 
    val run : (E.appinput, E.appoutput) sf -> float option -> unit
  end
