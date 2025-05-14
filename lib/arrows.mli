(*****************************************************************************)
(*                                                                           *)
(*                                  OCamlFRP                                 *)(*                                                                           *)
(* Copyright (C) 2025 Frédéric Dabrowski                                     *)
(* Copyright (C) 2025 Nicolas Paul                                           *)
(* All rights reserved.  This file is distributed under the terms of         *)
(* the GNU Lesser General Public License version 3.                          *)
(* You should have received a copy of the GNU General Public License         *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.    *)
(*****************************************************************************)

open Stream

(** {1 Synchronous functions} *)

(** Synchronous function type *)
type ('a, 'b) sf =
  | SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

(** [arr f] is the synchronous function constructed from function [f]. *)
val arr : ('a -> 'b) -> ('a,'b) sf

(** [f >>> g] is the synchronous function composition of synchronous functions
    [f] and [g]. *)
val ( >>> ) : ('a, 'b) sf -> ('b,'c) sf -> ('a, 'c) sf

(** [first f] is the synchronous function which applies only to the first
    element of a pair. *)
val first : ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf

(** [second f] is the synchronous function which applies only to the second
    element of a pair. *)
val second : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf

(** [parallel f g] is the synchronous function which applies synchronous
    function [f] on the first element of a pair, and synchronous function [g]
    on the second. *)
val parallel : ('a, 'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf 

(** [fanout f g] is the synchronous function which applies synchronous
    functions [f] and [g] on a single entry, returning a pair of both
    results [(f x, g x)] *)
val fanout : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf

(** [left f e] is the synchronous function which applies synchronous function
    [f] to [Either.Left] [e]. *)
val left : ('a, 'b) sf -> (('a, 'c) Either.t, ('b, 'c) Either.t) sf

(** [right f e] is the synchronous function which applies synchronous function
    [f] to [Either.Right] [e]. *)
val right : ('a, 'b) sf -> (('c, 'a) Either.t, ('c, 'b) Either.t) sf

(** [choice f g] is the synchronous function which applies synchronous
    function [f] on [Either.Left] [e] or synchronous function [g] on
    [Either.Right] [e]. *)
val choice : ('a, 'b) sf -> ('c, 'd) sf -> 
             (('a, 'c) Either.t, ('b, 'd) Either.t) sf

(** [fanin f g] is the synchronous function which consumes an [Either.t]
    and applies on the value synchronous function [f] if it was an
    [Either.Left] or synchronous function [g] if it was an [Either.Right]. *)
val fanin : ('a, 'b) sf -> ('c, 'b) sf -> (('a, 'c) Either.t, 'b) sf

(** [loop f x0] is the synchronous function which calls itself with the
    previous state, starting with state [x0]. *)
val loop : ('a * 'x, 'b * 'x) sf -> 'x -> ('a, 'b) sf

(** Iterate for synchronous functions *)
(** [lift f s] is the stream produced by applying synchronous function
    [f] to stream [s]. *)
val lift : ('a,'b) sf -> 'a stream -> 'b stream

(* TODO(nico): delete this nested module, or wrap everything in
   a module Arrows to have the SF type be named [t] to follow
   popular naming convention [Module.t] ??? *)

module Arr : sig
    (** [id] is the synchronous function which returns its arguments. *)
    val id : ('a, 'a) sf

    (** [const x] is the synchronous function which always returns [x]. *)
    val const : 'b -> ('a, 'b) sf

    (** [dup] is the synchronous function which returns a pair
        of its arguments. *)
    val dup : ('a, 'a * 'a) sf

    (** [delay t] *)
    val delay : 'a -> ('a, 'a) sf
end
