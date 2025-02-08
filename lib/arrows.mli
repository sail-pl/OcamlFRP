(*************************************************************************)
(*                                                                       *)
(*                                OCamlFRP                               *)
(*                                                                       *)
(* Copyright (C) 2025  Frédéric Dabrowski                                *)
(* All rights reserved.  This file is distributed under the terms of     *)
(* the GNU Lesser General Public License version 3.                      *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(*************************************************************************)

open Stream

(** {1 Stream functions} *)

(** Type of synchronous functions which consumes values of type [a]
    and produces values of type [b] *)  

(** Synchronous function type *)
type ('a,'b) sf = SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

(** Lift a function to a synchronous function *)
val arr : ('a -> 'b) -> ('a,'b) sf

(** Composition for synchronous functions *)
val (>>>) : ('a, 'b) sf -> ('b,'c) sf -> ('a, 'c) sf

(** First projection for synchronous functions *)
val first : 'a  'b 'c. ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf

(** Second projection for synchronous functions *)
val second : ('a,'b) sf -> ('c * 'a, 'c * 'b) sf

(** Parallel composition for synchronous functions *)
val parallel : ('a, 'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf 

(** Fanout for synchronous functions *)
val fanout : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf

(** Left projection for synchronous functions *)
val left : ('a, 'b) sf -> (('a, 'c) Either.t, ('b, 'c) Either.t) sf

(** Right projection for synchronous functions *)
val right : ('a, 'b) sf -> (('c, 'a) Either.t, ('c, 'b) Either.t) sf

(** Choice for synchronous functions *)
val choice : ('a, 'c) sf -> ('b, 'd) sf -> (('a, 'b) Either.t, ('c, 'd) Either.t) sf

(** Fanin for synchronous functions *)
val fanin : ('a, 'c) sf -> ('b, 'c) sf -> (('a, 'b) Either.t, 'c) sf

(** Loop for synchronous functions *)
val loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf

(** Iterate for synchronous functions *)
val lift : ('a,'b) sf -> 'a stream -> 'b stream


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
