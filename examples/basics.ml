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

open Format
open Fmt
open Ocamlfrp.Stream
open Ocamlfrp.Arrows

(* STREAMS *)

(** stream of () *)

let dummy : unit stream = stream (fun () -> ((),())) () 
(** stream of positive integers *)

let positives : int stream = stream (fun n -> (n, n+1)) 0
  (* coiterate ((+) 1) 0 *)

(* STREAM FUNCTIONS WITHOUT LOOPS *)

let identity : ('a,'a) sf = arr Fun.id

let plus_left : (int, int * int) sf = 
  arr Ocamlfrp.Utils.dup >>> first (arr (( + ) 1)) 

let squares : (int,int) sf = 
  arr Ocamlfrp.Utils.dup >>> (arr (Ocamlfrp.Utils.uncurry ( * )))

(* STREAM FUNCTIONS WITH LOOPS *)

let counter : ('a, int) sf = 
  let open Ocamlfrp.Utils in 
    loop (arr (mapright (( + ) 1) << dup << snd)) 0
  
let pre : 'a -> ('a,'a) sf = 
  loop (arr Ocamlfrp.Utils.swap)

let sum : (int, int) sf = 
  let open Ocamlfrp.Utils in 
  loop (arr (dup << uncurry ( + ))) 0 
  

let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "positives:") (list_of_stream (lift identity positives) 10) 
let _ = Ocamlfrp.Utils.show (pair pp_print_int pp_print_int) (Some "plust_left:")  (list_of_stream (lift plus_left positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "squares positives:") (list_of_stream (lift squares positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "counter:") (list_of_stream (lift counter dummy) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "pre positives:") (list_of_stream (lift (pre 0) positives) 10)
let _ = Ocamlfrp.Utils.show (pp_print_int) (Some "sum positives:")(list_of_stream (lift sum positives) 10)
let _ = Format.fprintf std_formatter "done.\n" 
let _ =   print_newline ();



