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
open FrpEngine

module Std : Environment with type appinput = string and type appoutput = string = 
  struct 
    type appinput = string 
    type appoutput = string 
    let init _ = ()
    let stop _ = ()
    let input _ = read_line ()
    let output s = print_endline s; true
  end 

module E = Engine(Std)

let _ = 
  let f (x, y) = Utils.dup (y ^ x) in
    E.run (loop (arr f) "") (Some 0.001)
