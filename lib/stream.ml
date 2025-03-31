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

type 'a stream = Str : ('state -> 'a * 'state) * 'state -> 'a stream

let destr (s : 'a stream) : 'a * 'a stream =
  let (Str (gen, init)) = s in
  let value, next = gen init in
  value, Str (gen, next)
;;

let head (s : 'a stream) : 'a = fst (destr s)
let tail (s : 'a stream) : 'a stream = snd (destr s)

let map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  let (Str (gen, init)) = s in
  let new_gen state =
    let head, next = gen state in
    f head, next
  in
  Str (new_gen, init)
;;

let apply (fs : ('a -> 'b) stream) (vs : 'a stream) : 'b stream =
  let (Str (fs_gen, fs_init)) = fs in
  let (Str (vs_gen, vs_init)) = vs in
  let new_gen =
    fun (fn_state, val_state) ->
    let f, fs_next = fs_gen fn_state in
    let v, vs_next = vs_gen val_state in
    f v, (fs_next, vs_next)
  in
  let new_init = fs_init, vs_init in
  Str (new_gen, new_init)
;;

let produce (f : 's -> 'a * 's) (state : 's) : 'a stream = Str (f, state)

let coiterate (f : 'a -> 'a) (init : 'a) : 'a stream =
  let gen state =
    let next = f state in
    state, next
  in
  produce gen init
;;

let constant (x : 'a) : 'a stream = coiterate (Fun.const x) x

let rec perform (s : 'a stream) (f : 'a -> unit) (n : int) : unit =
  if n <= 0
  then ()
  else (
    let (Str (gen, init)) = s in
    let value, next = gen init in
    (* TODO(nico): does it assumes f will have side effects? if so, should be documented! *)
    f value;
    perform (Str (gen, next)) f (n - 1))
;;

let rec consume (s : 'a stream) (predicate : 'a -> bool) (delay : float option) : unit =
  let (Str (gen, init)) = s in
  let value, next = gen init in
  let bool = predicate value in
  (* TODO(nico): not sure about the pattern matching on the delay here, shouldn't we still
     consume s later even tho no delay is provided? *)
  match delay with
  | None -> ()
  | Some t ->
    Thread.delay t;
    if bool then consume (Str (gen, next)) predicate delay
;;

let stream_of_list (l : 'a list) (a : 'a) : 'a stream =
  let gen = function
    | [] -> a, []
    | h :: t -> h, t
  in
  Str (gen, l)
;;

let rec list_of_stream (s : 'a stream) (n : int) =
  let (Str (gen, init)) = s in
  if n >= 0
  then (
    let value, next = gen init in
    value :: list_of_stream (Str (gen, next)) (n - 1))
  else []
;;
