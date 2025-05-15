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

open Ocamlfrp.Arrows
open Ocamlfrp.FrpEngine

(* A simple simulation of a game where you have to guess
   a number between 0 and 10 (included). *)

module Env =
  struct
    type guess = int

    type result =
      | Higher
      | Lower
      | Correct

    type appinput = guess
    type appoutput = result

    let init _ =
      print_endline "Welcome to the game Guess the Number, where you have to" ;
      print_endline "find the correct number chosen by the machine between 0" ;
      print_endline "and 10." ;
      print_endline "" ;
      print_endline "" ;
      print_endline "Your guess?" ;
      ()

    let stop _ =
      print_endline "" ;
      print_endline "" ;
      print_endline "Game over..." ;
      ()

    let input _ =
      print_string ">>> " ;
      let guess = int_of_string (read_line ()) in
      (* TODO(nico): check if the guess is in boundaries, otherwise re-ask? *)
      guess

    let output =
      function
      | Higher ->
        print_endline "Higher!" ;
        true
      | Lower ->
        print_endline "Lower!" ;
        true
      | Correct ->
        print_endline "That's right!" ;
        stop () ;
        false
  end

module E = Engine (Env)

let check =
  let f (guess, secret)= 
    let open Env in
    if guess = secret then
      Correct, secret
    else if guess > secret then
      Lower, secret
    else
      Higher, secret
  in
  arr f

let _ =
  Random.self_init () ;
  let secret = Random.int 11 (* bound is exclusive *) in
  let game = loop check secret in
  E.run game (Some 0.01)

