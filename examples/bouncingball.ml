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


let gravity = 0.5;; (* Accélération due à la gravité *)
let elasticity = 0.8;; 

(* state of the ball, rendering *)

type ball = {
  x : int;
  y : int;
  radius : int;
  color : color;
  velocity : float;
}

let mk_graphic : 'a 'b 's.
  ('a * 's, 'b * 's) sf -> 
    ('s -> renderObject) -> 
      ('a * 's, ('b * renderObject list) *'s) sf =
        fun sf mk_renderObject ->
          sf >>> arr (fun (b,s) -> ((b, [mk_renderObject s]),s))

let ball : 'a 'b. ('a * ball -> 'b * ball) -> 
  ('a * ball, ('b * renderObject list) * ball) sf 
  = 
  fun f ->
  mk_graphic 
    (arr f) 
    (fun b -> 
      mk_renderObject 
        (fun () -> set_color b.color; fill_circle b.x b.y b.radius))

let statefull_ball f s = loop (ball f) s
  
let () = 
    let t = fun (x, ball) -> 
      let new_velocity_y = ball.velocity -. gravity in
      let new_ball_y = ball.y + int_of_float new_velocity_y in
      let final_velocity_y, final_ball_y = 
        if new_ball_y - ball.radius <= 0 then
          (-.new_velocity_y *. elasticity, ball.radius) 
        else
          (new_velocity_y, new_ball_y)
        in
      (x, 
        { x = ball.x; 
          y = final_ball_y; 
          radius=ball.radius; 
          color=ball.color;
          velocity = final_velocity_y}) 
    in 
    let s = {x = 320; y= 400; radius = 10; color= black; velocity=0.0} in
    let _ = s.velocity in
    let b = statefull_ball t s >>> arr snd in
    run b (Some 0.0001)



