(*************************************************************************)
(*                                                                       *)
(*                                OCamlFRP                               *)
(*                                                                       *)
(* Copyright (C) 2025  Frédéric Dabrowski                                *)
(* All rights reserved.  This file is distributed under the terms of      *)
(* the GNU Lesser General Public License version 3.                      *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(*************************************************************************)

open Graphics
open Ocamlfrp.Stream
open Ocamlfrp.Arrows
open Ocamlfrp.Frpgraphics

type ball = {
  x : float; y : float;
  vx : float; vy : float;
  radius : float; color : color;}

let update : (input * ball, ball) sf =
  let f = fun (_, b) ->  
    let x = b.x +. b.vx and y = b.y +. b.vy in
    let vx, vy = 
      if x < b.radius || x >=  float_of_int 640 -. b.radius 
        then -. b.vx, b.vy
      else if y < b.radius || y >= float_of_int 480 -. b.radius 
        then b.vx, -. b.vy
      else b.vx, b.vy
    in {x = x; y = y; vx = vx; vy = vy; radius = b.radius; color = b.color}
  in arr f

let collide : (ball * ball, ball * ball) sf =
  let f = fun (b1, b2) -> 
    let dx = b1.x -. b2.x and dy = b1.y -. b2.y in
    let distance = sqrt (dx *. dx +. dy *. dy) in
    if distance < b1.radius +. b2.radius then
      {x = b1.x; y = b1.y; vx = b2.vx; vy = b2.vy; radius = b1.radius; color = b1.color},
      {x = b2.x; y = b2.y; vx = b1.vx; vy = b1.vy; radius = b2.radius; color = b2.color}
    else b1, b2
  in arr f
    
let render_ball : (ball, scene) sf = 
  arr (
    fun b ->
      let x,y,r = int_of_float b.x, int_of_float b.y, int_of_float b.radius in
        [{draw = fun () -> set_color b.color; fill_circle x y r}])

let world : ball * ball -> (input, output) sf = 
  let split_input = arr (fun (i, (b1,b2)) -> (i,b1), (i,b2)) in
  let merge_outputs = arr (fun (x,y) -> x@y) in
  let update2 = parallel update update in 
  let renderer2 = parallel render_ball render_ball  in 
  loop (split_input  >>> update2 >>> collide >>> Arr.dup >>> first (renderer2 >>> merge_outputs))
    
let _ = 
  let b1 = {x=100.; y=100.; vx=2.0; vy=2.0; radius=10.; color=black} in
  let b2 = {x=200.; y=200.; vx=(-2.0); vy=(-1.0); radius=10.; color=black} in
  run (world (b1,b2)) (Some 0.0001)

