(** {1 Streams} *)
open Coiterators

(** ['a stream] represents a type of stream of values of type ['a], 
    which is an infinite sequence of values of type ['a]. *)
type 'a stream = Str : ('a, 's) co -> 'a stream

val head : 'a stream -> 'a
  
val tail : 'a stream -> 'a stream 

(** [coiterate f s] constructs a stream using the coiterator [(f, s)], 
    where [f] is the function and [s] is the initial state. *)
val produce : ('s -> 'a * 's) -> 's -> 'a stream

(** [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x]. s*)
    val coiterate : 'a. ('a -> 'a) -> 'a -> 'a stream
    
(** [constant x] returns a constant stream where every element has the value x. *)
    val constant : 'a. 'a -> 'a stream
    
(** [list_of_stream s n] returns the first [n] elements of [s] as a list. *)
    val list_of_stream : 'a stream -> int -> 'a list
    val perform : 'a stream -> ('a -> unit) -> int -> unit

    val consume : 'a stream -> ('a -> bool) -> float option -> unit
    (* hd, tl ?? *)
