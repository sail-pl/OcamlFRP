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

open Stream   

type ('a, 'b) sf = SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

let create f s = SF (f, s)

let arr f = create (fun () x -> f x, ()) ()

let ( >>> ) (SF (f, sf)) (SF (g, sg)) =
  let h (s1, s2) x =
    let y, s1' = f s1 x in
    let z, s2' = g s2 y in
    z, (s1', s2')
  in create h (sf, sg)

let first (SF (f, s)) = create (fun s (x, z) -> let y, s' = f s x in (y, z), s') s
    
let second (SF (f, s)) = create (fun s (z, x) -> let y, s' = f s x in (z, y), s') s

let parallel (SF (f, sf)) (SF (g, sg))  = 
  let h (s1, s2) (x, y) =  
    let x', s1' = f s1 x in
    let y', s2' = g s2 y in 
    (x', y'), (s1', s2')
  in create h (sf, sg)

let fanin (SF (f, sf)) (SF (g, sg)) =
  let h (s1, s2) = function
    | Either.Left x ->
      let x', s1' = f s1 x in x', (s1', s2)
    | Either.Right x ->
      let x', s2' = g s2 x in x', (s1, s2')
    in create h (sf, sg)

let fanout (SF (f, sf)) (SF (g, sg)) =
  let h (s1, s2) x =
    let x1, s1' = f s1 x in
    let x2, s2' = g s2 x in
    (x1, x2), (s1', s2')
  in create h (sf, sg)

let left (SF (f, s)) =
  let g s = function
    | Either.Left x ->
      let x', s' = f s x in Either.Left x', s' 
    | Either.Right _ as x -> x, s 
  in create g s

let right (SF (f, s)) =
  let g s = function
    | Either.Left _ as x -> x, s
    | Either.Right x ->
      let x', s' = f s x in Either.Right x', s'
    in create g s

let choice (SF (f, sf)) (SF (g, sg)) =
  let h (s1, s2) = function
    | Either.Left x ->
      let x', s1' = f s1 x in Either.Left x', (s1', s2)
    | Either.Right x ->
      let x', s2' = g s2 x in Either.Right x', (s1, s2')
  in create h (sf, sg)

let loop (SF (f, s)) x0 = create (fun (s, c) a -> let (b, c'), s' = f s (a, c) in (b, (s', c'))) (s, x0)

let lift (SF (f, sf)) (Stream (t, st)) =
  let t (s2, s1) =
    let (a, s2') = t s2 in 
    let (b, s1') = f s1 a in 
    (b, (s2', s1'))
  in produce t (st, sf)

module Arr =
  struct 
    let id = SF ((fun s x -> x, s), ())
  
    let const x = arr (Fun.const x)

    let dup = SF ((fun s x -> (x, x), s), ())

    let delay t = loop (arr Utils.swap) t
  end