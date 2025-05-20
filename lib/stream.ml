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

type 'a stream = Stream : ('s -> ('a * 's)) * 's -> 'a stream

let coiterate f x0 = Stream ((fun x -> x, f x), x0)

let destr (Stream (f, s)) = let x, s' = f s in x, Stream (f, s')

let head stream = fst (destr stream)

let tail stream = snd (destr stream)
  
let map f (Stream (t, q)) = Stream ((fun s -> let x, s' = t s in f x, s'), q)

let apply (Stream (f, sf)) (Stream (g, sg)) =
  let h (s1, s2) =
    let x, s1' = f s1 in
    let y, s2' = g s2 in
    (x y), (s1', s2')
  in
  Stream (h, (sf, sg))

let constant x = coiterate (Fun.const x) x
  
let rec perform (Stream (g, s)) f n =
  if n <= 0 then ()
  else
    let x, s' = g s in
    f x ;
    perform (Stream (g, s')) f (n - 1)

let rec consume (Stream (f, s)) p d =
  let x, s' = f s in
  if p x then (* p might have side-effects (see Engine) -- nico *)
    match d with
      | None -> ()
      | Some timer ->
        Thread.delay timer ;
        consume (Stream (f, s')) p d

let stream_of_list l x = Stream ((function | [] -> x, [] | h :: t -> h, t), l)

let rec list_of_stream (Stream (f, s)) n =
  if n > 0 then
    let x, s' = f s in
    x :: (list_of_stream (Stream (f, s')) (n - 1))
  else []
