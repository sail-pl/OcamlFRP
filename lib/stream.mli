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

(** {1 Streams} *)

(** ['a stream] represents an infinite sequence of values of type ['a].*)
type 'a stream =
  | Stream : ('s -> ('a * 's)) * 's -> 'a stream

(** [destr s] is a pair containing the current value ['a] of stream [s] and
    the rest of [s]. *)
val destr : 'a stream -> 'a * 'a stream
(* TODO(nico): rename destr into "next" or something more explicit? *)

(** [head s] is the current value of stream [s]. *)
val head : 'a stream -> 'a
  
(** [tail s] is the rest of stream [s] without its current value. *)
val tail : 'a stream -> 'a stream 

(** [map f s] is the stream in which function [f] is applied to
    values of stream [s]. *)
val map : ('a -> 'b) -> 'a stream -> 'b stream
  
(** [apply fs s] is the stream of functions in stream [fs] applied
    to values of stream [s]. *)
val apply : ('a -> 'b) stream -> 'a stream -> 'b stream
  
(** [produce f s] is the stream with transition function [f]
    and initial state [s]*)
val produce : ('a -> 'b * 'a) -> 'a -> 'b stream

(* TODO(nico): ask F. Dabrowski about this, we may want to add a fold function 
(** [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x]. s*)
*)

(** [coiterate f s] constructs a stream using the coiterator [(f, s)], 
    where [f] is the function and [s] is the initial state. *)
val coiterate : ('a -> 'a) -> 'a -> 'a stream
    
(** [constant x] returns a constant stream where every element has the value x. *)
val constant : 'a -> 'a stream

(** [perform s f n] applies function [f] on [n] values of
    stream [s].  Function [f] can have side-effects. *)
val perform : 'a stream -> ('a -> unit) -> int -> unit

(** [consume s p d] consumes values from stream [s] as long
    as the values satisfy predicate [p].  An optional delay
    [d] can be passed to wait between each consumption. *)
val consume : 'a stream -> ('a -> bool) -> float option -> unit

(** [stream_of_list l x] is the stream of elements of list [l] with
    sentinel value [x] once there is no more elements from [l]. *)
val stream_of_list : 'a list -> 'a -> 'a stream

(** [list_of_stream s n] is a list of the [n]-last values from stream [s]. *)
val list_of_stream : 'a stream -> int -> 'a list