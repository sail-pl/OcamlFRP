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

open Stream   

type ('a, 'b) sf = 
  SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

let arr : ('a -> 'b) -> ('a, 'b) sf = 
  fun f ->
    let t, s = (fun () a -> (f a, ())), () in
      SF (t, s)

let (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf =
    fun (SF (t1, s1)) (SF (t2, s2)) -> 
      let t = 
          fun (s1, s2) a -> 
            let (b, s1') = t1 s1 a in 
              let (c, s2') = t2 s2 b in 
                (c, (s1', s2'))
      and s = (s1, s2) in 
        SF (t, s)

let first : ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf =
  fun (SF (t, s)) -> 
    let t = 
      fun s (a, c) -> 
        let (b, s') = t s a in ((b, c), s')
    in SF (t,s)
    
let second : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf =
  fun (SF (t, s)) -> 
    let t = 
      fun s (c, a) -> 
        let (b, s') = t s a in ((c, b), s')
    in SF (t, s)
    
let parallel : ('a, 'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf = 
  fun (SF (t1, s1)) (SF (t2, s2)) ->
    let t =  
      fun (s1, s2) (a, c) -> 
        let (b, s1') = t1 s1 a
        and (d, s2') = t2 s2 c in 
          ((b, d), (s1', s2'))
    in SF (t, (s1, s2))

let fanout : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf =
    fun (SF (t1, s1)) (SF (t2, s2)) ->
      let t = 
        fun (s1, s2) a -> 
          let (b, s1') = t1 s1 a
          and (c, s2') = t2 s2 a in 
              ((b, c), (s1', s2'))
        in SF (t, (s1, s2))    
            
let left : 
  ('a, 'b) sf -> 
    (('a, 'c) Either.t, ('b, 'c) Either.t) sf = 
  fun (SF (t, s)) -> 
    let t = fun s -> 
      function 
        | Either.Left a -> 
            let (b, s') = t s a in 
              (Either.Left b, s')
        | Either.Right c -> (Either.Right c, s)
    in SF (t, s)

let right : 
  ('a, 'b) sf -> 
    (('c, 'a) Either.t, ('c, 'b) Either.t) sf = 
  fun (SF (t, s)) -> 
    let t = fun s -> 
      function 
        | Either.Left c -> (Either.Left c, s)
        | Either.Right a -> 
            let (b, state') = t s a in 
              (Either.Right b, state')
    in SF (t, s)

let choice : 
  ('a, 'c) sf -> ('b, 'd) sf -> 
    (('a, 'b) Either.t, ('c, 'd) Either.t) sf = 
  fun (SF (t1, s1)) (SF (t2, s2)) -> 
    let t = 
      fun (state1, state2) -> 
        function 
          | Either.Left a -> 
              let (c, state') = t1 s1 a in 
                (Either.Left c, (state', state2))
          | Either.Right b -> 
            let (d, state') = t2 s2 b in 
              (Either.Right d, (state1, state'))
          in 
            SF (t, (s1, s2))

let fanin : 
  ('a, 'c) sf -> ('b, 'c) sf -> 
      (('a, 'b) Either.t, 'c) sf =
  fun (SF (t1, s1)) (SF (t2, s2)) -> 
    let t = 
      fun (s1, s2) -> 
        function 
          | Either.Left a -> 
            let (b, s') = t1 s1 a in 
              (b, (s', s2))
          | Either.Right c -> 
            let (b, s') = t2 s2 c in 
              (b, (s1, s'))
    in SF (t, (s1, s2))
        
let loop : ('a * 'c, 'b * 'c) sf -> 'c -> ('a, 'b) sf =
  fun (SF (t, s)) c ->
    let t = 
      fun (state, c) a ->
        let (b, c'), state' = t state (a, c) in  
          (b, (state', c'))
    in SF (t, (s, c))

let lift : ('a, 'b) sf -> 'a stream -> 'b stream =
  fun (SF (t1, s1)) -> 
    fun (Stream (t2, s2)) -> 
      let t = fun (state2, state1) -> 
        let (a, state2') = t2 state2 in 
          let (b, state1') = t1 state1 a in 
            (b, (state2', state1'))
      in Stream (t, (s2, s1))

module Arr = struct 

  let id : ('a, 'a) sf = 
    let t = fun s a -> (a, s) in SF (t, ())
  
  let const : 'b -> ('a, 'b) sf = 
    fun b -> 
      let t = fun s _ -> (b, s) in SF (t, ())
  
  let dup : ('a, 'a * 'a) sf = 
    let t = fun s a -> ((a, a), s) in SF (t, ())

  let delay : 'a -> ('a, 'a) sf = 
    fun a -> loop (arr Utils.swap) a

end