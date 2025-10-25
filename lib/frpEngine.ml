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

open Stream

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

module Engine (E : Environment)= 
  struct
    let run (f : (E.appinput, E.appoutput) sf) (d : float option) = 
        E.init ();
        let s = produce (fun () -> (E.input false,())) ()  in 
          consume (lift f s) E.output d
  end