open Stream

(** {1 Stream functions} *)

(** Type of synchronous functions which consumes values of type [a]
    and produces values of type [b] *)  
type ('a,'b) sf

(** Lift a function to a synchronous function *)
val arr : ('a -> 'b) -> ('a,'b) sf

(** Composition of synchronous functions *)
val (>>>) : ('a, 'b) sf -> ('b,'c) sf -> ('a, 'c) sf

val first : 'a  'b 'c. ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf
val second : ('a,'b) sf -> ('c * 'a, 'c * 'b) sf
val parallel : ('a, 'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf 
val fanout : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf

val left : ('a, 'b) sf -> (('a, 'c) Either.t, ('b, 'c) Either.t) sf
val right : ('a, 'b) sf -> (('c, 'a) Either.t, ('c, 'b) Either.t) sf
val choice : ('a, 'c) sf -> ('b, 'd) sf -> (('a, 'b) Either.t, ('c, 'd) Either.t) sf
val fanin : ('a, 'c) sf -> ('b, 'c) sf -> (('a, 'b) Either.t, 'c) sf

val loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf

val lift : ('a,'b) sf -> 'a stream -> 'b stream

val sf_of_stream : 'a stream -> ('b,'a) sf

type 'a event = 'a option

(*
    arr (id) >>> a = a
    a >>> arr (id) = a
    a >>> (b >>> c) = (a >>> b) >>> c 
    arr (g o f) = arr f >>> arr g
    first (a) >>> arr (pi1) = arr (pi1) >>> a
    first a >>> arr (id * f) = arr (id * f) >>> first a
    first a >>> arr f = arr f >>> first (first a)
    first (arr f) = arr (f * id)
    first (a >>> b) = first a >>> first b *)
