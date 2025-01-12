(** {1 Streams} *)

(** ['a stream] represents a type of stream of values of type ['a], 
    which is an infinite sequence of values of type ['a]. *)
    type 'a stream

(** [coiterate f s] constructs a stream using the coiterator [(f, s)], 
    where [f] is the function and [s] is the initial state. *)
val coiterate : ('s -> 'a * 's) -> 's -> 'a stream

(** [fold f x] generates a stream by iteratively applying the function [f] 
    to the current value, starting with [x]. s*)
    val fold : 'a. ('a -> 'a) -> 'a -> 'a stream
    
(** [constant x] returns a constant stream where every element has the value x. *)
    val constant : 'a. 'a -> 'a stream
    
(** [list_of_stream s n] returns the first [n] elements of [s] as a list. *)
    val list_of_stream : 'a stream -> int -> 'a list

    (* hd, tl ?? *)

(** {1 Stream functions} *)

(** [('a,'b) sf] is the type of stream functions which    
    consumes streams of type ['a] and produce streams of type ['b] *)
type ('a,'b) sf

val apply : ('a,'b) sf -> 'a stream -> 'b stream

(** The stream function [arr f] applies the function [f] to each element of a stream.
    It satisfies the following equations :
   - head ((arr f) s) = f (head s) 
   - tail ((arr f) s) = arr f (tail s) *)
val arr : 'a 'b. ('a -> 'b) -> ('a,'b) sf

(** The unary [first] operator applies a stream function to the left members of a stream of pairs.*)
val first : 'a  'b 'c. ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf

val (>>>) : ('a, 'b) sf -> ('b,'c) sf -> ('a, 'c) sf

(** The unary [loop] operator plugs a stream function to a register *)            
val loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf

val second : ('a,'b) sf -> ('c * 'a, 'c * 'b) sf
  
val parallel : ('a,'b) sf -> ('c,'d) sf -> ('a *'c, 'b * 'd) sf
  
val fork : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf 
