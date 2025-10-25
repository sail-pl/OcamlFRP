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

(** {1 Streams} *)

(** ['a stream] represents a type of stream of values of type ['a], 
    which is an infinite sequence of values of type ['a]. *)

type 'a stream (* = Str : ('s -> ('a * 's)) * 's -> 'a stream*)

val stream : ('c -> 'a *'c) -> 'c -> 'a stream

val map : ('a -> 'b) -> 'a stream -> 'b stream

val apply : 'a 'b. ('a -> 'b) stream -> 'a stream -> 'b stream

val coiterate : ('c -> 'a -> 'b * 'c) -> 'c -> 'a stream -> 'b stream

type ('a, 'b) sf = 
  SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

val id : ('a, 'a) sf

val compose : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf

val lift : ('a, 'b) sf -> 'a stream -> 'b stream 

val destr : 'a stream -> 'a * 'a stream

val head : 'a stream -> 'a
  
val tail : 'a stream -> 'a stream 
  

(** [coiterate f s] constructs a stream using the coiterator [(f, s)], 
    where [f] is the function and [s] is the initial state. *)
val produce : ('s -> 'a * 's) -> 's -> 'a stream

(* [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x]. s*)
    (* val coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream *)
    
(* [constant x] returns a constant stream where every element has the value x. *)
    (* val constant : 'a. 'a -> 'a stream *)
    
    val perform : 'a stream -> ('a -> unit) -> int -> unit

    val consume : 'a stream -> ('a -> bool) -> float option -> unit
    (* hd, tl ?? *)

val stream_of_list : 'a list -> 'a -> 'a stream
    
val list_of_stream : 'a stream -> int -> 'a list 
    