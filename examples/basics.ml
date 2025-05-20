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

(* This example presents the basic primitives of the
   OCamlFRP library, creating and using them in intuitive
   and familiar situations.  The goal of this example is
   mainly to serve as an introduction.  For anything more
   complex or advanced, please have a look at the various
   example files. *)

open Ocamlfrp.Arrows
open Ocamlfrp.Stream

(*** Streams *******************************************************************

     A stream is a representation of an infinite and continuous sequence
     of values.  You can think of it as the evolution of the value of a
     variable over a time period.  For instance, let's have [s] be the
     variable of the speed of a car, changing over time.  We can
     represent the different values of [s] as a stream:

                      +----+----+----+----+----+---------+
                      | s0 | s1 | s2 | s3 | s4 |   ...   |
                      +----+----+----+----+----+---------+

                            The stream "s" evolving
                             over a discrete time.
 
 
     Streams are constructed from a transition function and an initial
     value.  You can think of it as a automaton, transitioning from one
     state to another, with each transition being labelled by a value.
 
              type 'a stream = ('s -> ('a * 's)) * 's -> 'a stream
                               ^^^^^^^^^^^^^^^^^   ^^
                                               |    |
                                               |    +-- initial state
                   transition function --------+

     The transition function takes a state ['s] from which it gives a value
     ['a] and the next state ['s].  The way the value is created depends on
     your requirements: it may be a constant, a system of equations, or a
     value fetched from a physical and external sensor.
     The initial state is the origin of the stream, its lower bound.

     Such a construction avoids leaks over time and space, as well as
     performance costs, which are common in more classical implementations of
     functional reactive programming.

     **************************************************************************)

(* [ones] is a stream whose value will always be the integer 1.

   It is constructed from the constant function [x -> x, 1]
   which, for any input [x], will return [1], starting at [1]. *)

let ones = Stream ((fun x -> x, 1), 1)

let () =
  print_endline "*** ones ***" ;
  (* destr allows iterating over a stream one element at a time. *)
  let x, ones = destr ones in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, ones = destr ones in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, _ = destr ones in Printf.printf "x = %d\n" x ;    (* x = 1 *)
  ()

(* OCamlFRP provides primitive functions to construct various streams,
   including constant streams.  As such, the following [ones2] stream
   is equivalent to [ones]. *)

let ones2 = constant 1

let () =
  print_endline "*** ones2 ***" ;
  let x, ones2 = destr ones2 in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, ones2 = destr ones2 in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, _ = destr ones2 in Printf.printf "x = %d\n" x ;     (* x = 1 *)
  ()

(* But streams allow for more powerful constructions than trivial constant
   values!  Let's create [nat], the sequence of all natural and positive
  integers (including zero). 
  
  coiterate is an elegant way of constructing a stream using the concept of
  co-iterators.  It's more intuitive and simpler to play with. *)

let nat = coiterate ((+) 1) 0

let () =
  print_endline "*** nat ***" ;
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 0 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 2 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 3 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 4 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 5 *)
  let x, nat = destr nat in Printf.printf "x = %d\n" x ; (* x = 6 *)
  let x, _ = destr nat in Printf.printf "x = %d\n" x ;   (* x = 7 *)
  (* ... *)
  ()

(* You can apply a function over a stream, for instance, let's square all 
   natural numbers from our previous [nat] stream. *)

let sqr x = x * x

let sqrnat = map sqr nat

let () =
  print_endline "*** sqrnat ***" ;
  let x, sqrnat = destr sqrnat in Printf.printf "x = %d\n" x ; (* x = 0 *)
  let x, sqrnat = destr sqrnat in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, sqrnat = destr sqrnat in Printf.printf "x = %d\n" x ; (* x = 4 *)
  let x, sqrnat = destr sqrnat in Printf.printf "x = %d\n" x ; (* x = 9 *)
  let x, sqrnat = destr sqrnat in Printf.printf "x = %d\n" x ; (* x = 16 *)
  let x, _ = destr sqrnat in Printf.printf "x = %d\n" x ;      (* x = 25 *)
  (* ... *)
  ()

(* We can also applies functions from a stream of functions to values from
   a stream of values!  This is handy when trying to, for illustration,
   combine two streams: *)

let evens = coiterate ((+) 2) 0
(* =>   0   2   4   6   ... *)

let odds = coiterate ((+) 2) 1
(* =>   1   3   5   7   ... *)

let _pairs = apply (map (fun x y -> x, y) evens) odds
(* =>   (0, 1)   (2, 3)   (4, 5)   (6, 7)   ... *)
   
(* This also works with functions containing side-effects. *)

let tasks =
  let task x = 
    Printf.printf "Processing %d...\n%!" x;
    x * x, x + 1
  in
  Stream (task, 1)

let () =
  print_endline "*** tasks ***" ;
  let logger = Printf.printf "Result: %d\n" in
  perform tasks logger 3

(* Finally, you can switch from a stream to a list, or from a list to a stream
   without hassle. *)
let fib =
  coiterate (fun (x, y) -> y, x + y) (0, 1)
  |> map fst

let () =
  print_endline "*** fib/fib10 ***" ;
  let fib10 = list_of_stream fib 10 in (* first 10 fibonacci numbers *)
  let _ = List.map (Printf.printf "%d   \n") fib10 in
  ()

let hello = [ "w" ; "o" ; "r" ; "l" ; "d" ]

let () =
  print_endline "*** hello ***" ;
  let hs = stream_of_list hello "!" in print_string "hello, " ; (* "hello, " *)
  let c, hs = destr hs in print_string c ;                      (* "hello, w" *)
  let c, hs = destr hs in print_string c ;                      (* "hello, wo" *)
  let c, hs = destr hs in print_string c ;                      (* "hello, worl" *)
  let c, hs = destr hs in print_string c ;                      (* "hello, world" *)
  let c, hs = destr hs in print_string c ;                      (* "hello, world!" *)
  let c, hs = destr hs in print_string c ;                      (* "hello, world!!" *)
  let c, _ = destr hs in print_string c ;                       (* "hello, world!!!" *)
  (* ... *)
  print_endline "" ;
  ()


(*** Arrows ********************************************************************

     Arrows are a generalisation of functions.  They represent a computation of
     values of type ['a] to values of type ['b] while carrying an internal
     state.  We provide Synchronous Functions, or [SF], a form of arrows
     created for synchronous and reactive programs.

     Arrows can be composed, applied, recursively defined, or branched, allowing
     the construction of programs with similar properties to structured
     programming, while focusing on the data-flow, not the control flow.

     We can visualise the data-flow of values within a program constructed with
     arrows using diagrams, which may help understanding:

         x ---> arr f ---> f x

         x ---> f >>> g ---> g (f x)

         (x, y) ---> first f ---> (f x, y)
         (x, y) ---> parallel f g ---> (f x, g y)

     **************************************************************************)

let nat = Stream ((fun x -> x, x + 1), 0) 

(* arr lifts a pure function into a stateless arrow *)
let increment = arr (fun x -> x + 1)

let () =
  print_endline "*** inc ***" ;
  (* lift returns a stream from the application of an arrow on a stream *)
  let natincr = lift increment nat in 
  let x, natincr = destr natincr in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, natincr = destr natincr in Printf.printf "x = %d\n" x ; (* x = 2 *)
  let x, _ = destr natincr in Printf.printf "x = %d\n" x ;       (* x = 3 *)
  ()

(* You can compose arrows the chain them, like a pipeline. *)

let double = arr (( * ) 2)

let substract1 = arr (fun x -> x - 1)

let double_then_substract1 = double >>> substract1

let () =
  print_endline "*** double_then_substract1 ***" ;
  let s = lift double_then_substract1 nat in
  let x, s = destr s in Printf.printf "x = %d\n" x ; (* x = -1 *)
  let x, s = destr s in Printf.printf "x = %d\n" x ; (* x = 1 *)
  let x, _ = destr s in Printf.printf "x = %d\n" x ; (* x = 3 *)
  ()

(* You can also decide to apply synchronous function on pairs. *)

let double = arr (( * ) 2)

let add1   = arr (( + ) 1)

let pairs = coiterate (fun (x, y) -> (x + 1, y + 1)) (0, 0)

let () =
  print_endline "*** first double ***" ;
  let s = lift (first double) pairs in
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (0, 0) *)
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (2, 1) *)
  let (x, y), _ = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (4, 2) *)
  ()

let () =
  print_endline "*** second add1 ***" ;
  let s = lift (second add1) pairs in
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (0, 1) *)
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (1, 2) *)
  let (x, y), _ = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (2, 3) *)
  ()

let () =
  print_endline "*** parallel double add1 ***" ;
  let s = lift (parallel double add1) pairs in
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (0, 1) *)
  let (x, y), s = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (2, 2) *)
  let (x, y), _ = destr s in Printf.printf "(x, y) = (%d, %d)\n" x y ; (* (x, y) = (4, 3) *)
  ()

(* You can route values through conditionally chosen arrows using:
   
       Left  a : (Left x)  ---> Left (a x)
       Right a : (Right y) ---> Right (a y)
       Choice a b :
           Left x  ---> Left (a x)
           Right y ---> Right (b y)

   This is a way of achieving "if" statements.
*)

let square = arr (( * ) 2)

let negate = arr (( * ) (-1))

let eithers = 
  let open Either in
  stream_of_list [ Left 2 ; Right 3 ; Left 4 ; Right 5 ] (Right 0)

let print_either = function
  | Either.Left x -> Printf.printf "x = Left (%d)\n" x ;
  | Either.Right x -> Printf.printf "x = Right (%d)\n" x ;
  ()

let () =
  print_endline "*** left square ***" ;
  let s = lift (left square) eithers in
  let x, s = destr s in print_either x ; (* x = Left (4) *)
  let x, s = destr s in print_either x ; (* x = Right (3) *)
  let x, s = destr s in print_either x ; (* x = Left (8) *)
  let x, _ = destr s in print_either x ; (* x = Right (5) *)
  ()

let () =
  print_endline "*** right negate ***" ;
  let s = lift (right negate) eithers in
  let x, s = destr s in print_either x ; (* x = Left (2) *)
  let x, s = destr s in print_either x ; (* x = Right (-3) *)
  let x, s = destr s in print_either x ; (* x = Left (4) *)
  let x, _ = destr s in print_either x ; (* x = Right (-5) *)
  ()

let () =
  print_endline "*** choice square negate ***" ;
  let s = lift (choice square negate) eithers in
  let x, s = destr s in print_either x ; (* x = Left (4) *)
  let x, s = destr s in print_either x ; (* x = Right (-3) *)
  let x, s = destr s in print_either x ; (* x = Left (8) *)
  let x, _ = destr s in print_either x ; (* x = Right (-5) *)
  ()


(* Lastly, you can recursively define arrows so that they loop on themselves,
with a state that is remembered between each "iteration". *)

let sum = arr (fun (x, acc) -> let acc' = acc + x in acc', acc')

let () =
  print_endline "*** loop sum 0 ***" ;
  let n = stream_of_list [1 ; 2 ; 3 ; 4] 0 in
  let s = lift (loop sum 0) n in
  let x, s = destr s in Printf.printf "%d\n" x ; (* x = 1 *)
  let x, s = destr s in Printf.printf "%d\n" x ; (* x = 3 *)
  let x, s = destr s in Printf.printf "%d\n" x ; (* x = 6 *)
  let x, _ = destr s in Printf.printf "%d\n" x ; (* x = 10 *)
  ()


(*** Engines *******************************************************************

     **************************************************************************)

(* TODO(nico): tutorial on engines, but need to first discuss about their
   interface and refactoring them... *)

