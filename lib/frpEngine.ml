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

open Stream
open Arrows

(* NOTE(nico): there's no way to not duplicate this type definition
   since OCaml enforces having non-asbtract definition in implementation
   files... *)
module type Environment = 
  sig 
    type appinput
    type appoutput
    val init : unit -> unit
    val stop : unit -> unit 
    val input : bool -> appinput 
    val output : appoutput -> bool  
  end 

module Engine (E : Environment)= 
  struct
    let run f d =
      E.init () ;
      let s = produce (fun () -> E.input false, ()) () in
      consume (lift f s) E.output d
  end