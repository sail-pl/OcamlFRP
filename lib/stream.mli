(*************************************************************************)
(*                                                                       *)
(*                                OCamlFRP                               *)
(*                                                                       *)
(* Copyright (C) 2025  Frédéric Dabrowski                                *)
(* Copyright (c) 2025 Nicolas Paul                                       *)
(*                                                                       *)
(* All rights reserved.  This file is distributed under the terms of     *)
(* the GNU Lesser General Public License version 3.                      *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(*************************************************************************)

(** {1 Streams} *)

(** ['a stream] represents a type of stream of values of type ['a], which is an infinite
    sequence of values of type ['a].  It is composed of a generator (a function mapping
    a state to a value and the next state) and an initial state. *)
type 'a stream = Str : ('s -> 'a * 's) * 's -> 'a stream

(** [destr s] deconstructs a stream [s] into a pair containing its
    head and tail. *)
val destr : 'a stream -> 'a * 'a stream

(** [head s] returns the head of a stream. *)
val head : 'a stream -> 'a

(** [tail s] returns the tail of a stream. *)
val tail : 'a stream -> 'a stream

(** [map f s] applies function [f] to all the values of stream [s],
    and returns a stream with all the results returned by f. *)
val map : 'a 'b. ('a -> 'b) -> 'a stream -> 'b stream

(** [apply fs vs] constructs a stream from combining a stream of functions [fs]
    to a stream of values [vs] by applied the functions to their corresponding
    values. *)
val apply : 'a 'b. ('a -> 'b) stream -> 'a stream -> 'b stream

(** [produce f i] constructs a stream from a function [f] andß
    an initial state [i]. *)
val produce : ('s -> 'a * 's) -> 's -> 'a stream
(* TODO(nico): isnt procude redundant with the constructor? *)

(* TODO(nico): doc comment is there without any interface... ????
(** [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x]. s*)
*)

(** [coiterate f s] constructs a stream using the coiterator [(f, s)], 
    where [f] is the function and [s] is the initial state. *)
val coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream

(** [constant x] constructs a stream where every element has the value x. *)
val constant : 'a. 'a -> 'a stream

val perform : 'a stream -> ('a -> unit) -> int -> unit

(** [consume s p d] consumes stream [s] at least once, and more if the stream values
    validate predicate [p].  A delay [d] may be passed to wait between each
    consumption. *)
val consume : 'a stream -> ('a -> bool) -> float option -> unit

(** [stream_of_list l a] constructs a stream from the elements of a list [l]
    and then [a] once the list is entirely consumed, allowing the stream
    to be infinite. *)
val stream_of_list : 'a list -> 'a -> 'a stream

(** [list_of_stream s n] constructs a list from the [n] first elements of stream [s] *)
val list_of_stream : 'a stream -> int -> 'a list
