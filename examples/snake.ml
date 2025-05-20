(******************************************************************************)
(*                                                                            *)
(*                                  OCamlFRP                                  *)
(*                                                                            *)
(* Copyright (C) 2025 Nicolas Paul                                            *)
(* All rights reserved.  This file is distributed under the terms of          *)
(* the GNU Lesser General Public License version 3.                           *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.     *)
(******************************************************************************)

open Graphics

open Ocamlfrp.Arrows
open Ocamlfrp.FrpEngine

(* A simple simulation of the Snake game, in which you move a snake on a board,
   eating fruits to grow in length.  Avoid crashing on yourself, it hurts...

   Controls: WASD, ZQSD, Direction Arrows (soon)

   Best of luck. *)

module Env = struct
  type direction =
    | Up
    | Down
    | Left
    | Right
    | None
  
  (* We divide the plate using software-emulated pixels, we call them cells, 
     and they represent positions for apples and the snake body.

     Therefore, we can imagine the plate of the game to be an eight by eight
     aligned grid:

         +----+----+----+----+----+----+----+----+
         | 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 |
         +----+----+----+----+----+----+----+----+
         | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 |
         +----+----+----+----+----+----+----+----+
         | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 |
         +----+----+----+----+----+----+----+----+
         | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |
         +----+----+----+----+----+----+----+----+
         | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |
         +----+----+----+----+----+----+----+----+
         | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 |
         +----+----+----+----+----+----+----+----+
         | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 |
         +----+----+----+----+----+----+----+----+
         | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |
         +----+----+----+----+----+----+----+----+

     Such a modelisation also removes our need to be careful of the
     rendered size of a cell in the game logic, such as our physic
     system.
     
     Unfortunately we cannot encode our invariant in code that
     0 <= cell <= 63, as OCaml doesn't yet have dependent types. *)

  type scene =
    | Game of state
    | End of int (* score *)
  and state =
    { snake     : cell list (* the snake body *)
    ; direction : direction (* current direction of the snake *)
    ; apple     : cell      (* position of the apple *)
    ; score     : int       (* score, number of apple eaten *)
    ; }
  and cell = int (* 0 <= cell <= 63 *)

  type appinput = direction
  type appoutput = scene

  (* TODO(nico): support window resizing? *)
  let width = 400
  let height = width
  let cell = width / 8

  let init () =
    open_graph (Printf.sprintf " %dx%d" width height) ;
    set_window_title "Snake" ;
    ()

  let stop () =
    close_graph () ;
    ()

  let _lastdir = ref Right

  let input _ =
    while key_pressed () do
      let dir = match read_key () with
      (* TODO(nico): direction arrows, but doesn't seems to work with
         Graphics, at least on OS X since they aren't even read... *)
        | 'w' | 'z' -> Up
        | 'a' | 'q' -> Left
        | 's'       -> Down
        | 'd'       -> Right
        | _         -> None
      in
      if dir <> None then _lastdir := dir ;
    done;
    !_lastdir

let output =
  function
    | Game state ->
      let draw_cell i j =
        set_color white ;
        fill_rect (j * cell) (height - (i + 1) * cell) cell cell ;
        set_color black ;
        draw_rect (j * cell) (height - (i + 1) * cell) cell cell ;
      in
      let draw_snake i c =
        let x = (c mod 8) * cell in
        let y = height - ((c / 8 + 1) * cell) in
        (* body in green, head in black *)
        set_color (if i = 0 then black else green) ;
        fill_rect x y cell cell
      in
      clear_graph () ;

      (* Draw the 8x8 grid board *)
      for i = 0 to 7 do
        for j = 0 to 7 do
          draw_cell i j
        done
      done ;

      (* Draw the snake *)
      List.iteri draw_snake state.snake ;

      (* Draw the apple *)
      let apple_x = (state.apple mod 8) * cell in
      let apple_y = height - ((state.apple / 8 + 1) * cell) in
      set_color red ;
      fill_rect apple_x apple_y cell cell ;

      synchronize () ;
      true

    | End score ->
      clear_graph () ;

      set_color white ;
      fill_rect 0 0 width height ;

      let center_text y_offset text =
        let text_width = fst (text_size text) in
        let x = (width - text_width) / 2 in
        let y = (height / 2) + y_offset in
        moveto x y ;
        draw_string text
      in

      set_color black ;
      center_text 10 "Game Over" ;
      center_text (-20) (Printf.sprintf "Score: %d" score) ;

      synchronize () ;
      Thread.delay 5. ;
      false

end

module E = Engine (Env)

let next_cell c =
  function
    | Env.Up -> (c + 56) mod 64
    | Env.Down -> (c + 8) mod 64
    | Env.Left -> if c mod 8 = 0 then c + 7 else c - 1
    | Env.Right -> if c mod 8 = 7 then c - 7 else c + 1
    | Env.None -> c

let calculate_snake_head =
  arr Env.(fun (dir, s) ->
    let dir = if dir = None then s.direction else dir in
    next_cell (List.hd s.snake) dir, dir, s)

let move_snake_head =
  arr (fun (head, direction, s) -> Env.{ s with snake = head :: s.snake ; direction })

let check_apple_collision =
  arr Either.(fun s -> if Env.(List.hd s.snake) = s.apple then Left s else Right s)

let increment_score =
  arr (fun state -> Env.{ state with score = state.score + 1})

let generate_apple =
  arr Env.(fun s ->
      let rec pick () = 
        let n = Random.int 64 in
        if List.mem n s.snake then pick () else n
      in { s with apple = pick () })

let remove_snake_tail =
  arr (fun s -> Env.{ s with snake = List.rev s.snake |> List.tl |> List.rev })

let make_scene =
  arr Env.(fun s -> if List.mem (List.hd s.snake) (List.tl s.snake) then End s.score else Game s)

let game =
  let initial_state = Env.{ snake = [24]; direction = Right; apple = 30; score = 0 } in
  loop
    (calculate_snake_head           (* determine the next snake position *)
      >>> move_snake_head           (* move its head over there *)
      >>> check_apple_collision     (* check if the snake ate an apple *)
      >>> choice                    (* and if so: *)
        (generate_apple               (* generate a new apple *)
          >>> increment_score)          (* and increase the score *)
        remove_snake_tail             (* otherwise remove the snake tail *)
      >>> fanin id id               (* consume the either to get the state *)
      >>> fanout                    (* and then: *)
        make_scene                    (* create the scene to be renderred *)
        id)                           (* and pass the state back to loop *)
    initial_state
  (* TODO(nico): manage if the board is full of the snake, since for now we will
     be stuck in an infinite loop (as the new apple position won't be solved),
     and we also do not end the game...  But hey, it's an example! *)

let _ =
  Random.self_init () ;
  E.run game (Some 0.1333)
  (* TODO(nico): there's some delay with the game caused by the FrpEngine
     design being monoclocked.  In a real simulation, you may instead have
     two engine, one to read the inputs from the keyboard, and one for the
     logic and renderer, and you may pass data between them in your
     input/output via channels.  This is left to be explored. *)
