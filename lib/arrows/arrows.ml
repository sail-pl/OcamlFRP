open Coiterators
open Stream   

(** * Signal functions, denote length-preserving synchronous functions. *)

type ('a, 'b) sf = 
  SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

(** Lift a function to a signal function *)

let arr : ('a -> 'b) -> ('a, 'b) sf = 
  fun f ->
    let step, state = (fun () a -> (f a, ())), () in
      SF (step, state)

(* Composition of synchronous functions *)

let (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf =
      let aux (step1, state1) (step2, state2)  = 
        (fun (state1, state2) a -> 
          let (b, state1') = step1 state1 a in 
            let (c, state2') = step2 state2 b in 
              (c, (state1', state2'))), 
        (state1, state2) in 
    fun (SF (step1, state1)) (SF (step2, state2)) -> 
      let (step, state) = aux (step1, state1) (step2, state2) in 
        SF (step, state)

(*  Processes the first component through given signal function 
    and leaves the second component unchanged. *)

let first : ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf =
  let aux (step, state) = 
    (
      (fun state (a, c) -> 
        let (b, state') = step state a in ((b, c), state')), 
      state )
  in 
    fun (SF (step, state)) -> 
      let (step, state) = aux (step, state) in SF (step, state)

(*  Processes the second component through given signal function 
    and leaves the first component unchanged. *)

let second : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf =
  let aux (step, state) = 
    (
      (fun state (c, a) -> 
        let (b, state') = step state a in ((c, b), state')), 
      state )
  in 
    fun (SF (step, state)) -> 
      let (step, state) = aux (step, state) in SF (step, state)

(* Parallel composition of two arrows *)
(* a b c -> a b' c' -> a (b, b') (c, c')  *)

let parallel : ('a, 'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf = 
      let aux (step1, state1) (step2, state2) = 
        (
          (fun (state1, state2) (a, c) -> 
            let (b, state1') = step1 state1 a in 
              let (d, state2') = step2 state2 c in 
                ((b, d), (state1', state2'))), 
          (state1, state2))
      in
        fun (SF (step1, state1)) (SF (step2, state2)) -> 
          let (step, state) = aux (step1, state1) (step2, state2) in 
            SF (step, state)

let fanout : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf =
  let aux (step1, state1) (step2, state2) = 
    (
      (fun (state1, state2) a -> 
        let (b, state1') = step1 state1 a in 
          let (c, state2') = step2 state2 a in 
            ((b, c), (state1', state2'))), 
      (state1, state2))
  in 
    fun (SF (step1, state1)) (SF (step2, state2)) -> 
      let (step, state) = aux (step1, state1) (step2, state2) in 
        SF (step, state)
            
(* left *)

let left : ('a, 'b) sf -> (('a, 'c) Either.t, ('b, 'c) Either.t) sf = 
  let aux (step, state) = 
    (
      (fun state -> 
        function 
          | Either.Left a -> 
            let (b, state') = step state a in (Either.Left b, state')
          | Either.Right c -> (Either.Right c, state)), 
      state)
  in 
    fun (SF (step, state)) -> 
      let (step, state) = aux (step, state) in 
        SF (step, state)

let right : ('a, 'b) sf -> (('c, 'a) Either.t, ('c, 'b) Either.t) sf = 
  let aux (step, state) = 
    (
      (fun state -> 
        function 
          | Either.Left c -> (Either.Left c, state)
          | Either.Right a -> 
            let (b, state') = step state a in (Either.Right b, state')), 
      state)
  in 
    fun (SF (step, state)) -> 
      let (step, state) = aux (step, state) in 
        SF (step, state)

let choice : ('a, 'c) sf -> ('b, 'd) sf -> (('a, 'b) Either.t, ('c, 'd) Either.t) sf = 
  let aux (step1, state1) (step2, state2)  = 
    (
      (fun (state1, state2) -> 
        function 
          | Either.Left a -> 
            let (c, state') = step1 state1 a in (Either.Left c, (state', state2))
          | Either.Right b -> 
            let (d, state') = step2 state2 b in (Either.Right d, (state1, state'))), 
      (state1, state2))
  in 
    fun (SF (step1, state1)) (SF (step2, state2)) -> 
      let (step, state) = aux (step1, state1) (step2, state2) in 
        SF (step, state)

(* Fanout operator *)

let fanin : ('a, 'c) sf -> ('b, 'c) sf -> (('a, 'b) Either.t, 'c) sf =
let aux (step1, state1) (step2, state2) = 
  (
    (fun (state1, state2) -> 
      function 
        | Either.Left a -> 
          let (b, state') = step1 state1 a in (b, (state', state2))
        | Either.Right c -> 
          let (b, state') = step2 state2 c in (b, (state1, state'))), 
    (state1, state2))
in 
  fun (SF (step1, state1)) (SF (step2, state2)) -> 
    let (step, state) = aux (step1, state1) (step2, state2) in 
      SF (step, state)
        
  
(** Loop operator : creates a feedback loop with an initial value *)

let loop : ('a * 'c, 'b * 'c) sf -> 'c -> ('a, 'b) sf =
  let aux (step, state) c = 
    (fun (state, c) a ->
      let (b, c'), state' = step state (a, c) in  
        (b, (state', c'))), (state, c) 
    in fun (SF (step, state)) c ->
      let (step, state) = aux (step, state) c in 
        SF (step, state)

(* *)

(** Lifts a synchronous function to operate on streams *)

let lift : ('a, 'b) sf -> 'a stream -> 'b stream =
    let aux (step1, state1) (step2, state2)  = 
      (
          (fun (state2, state1) -> 
            let (a, state2') = step2 state2 in 
              let (b, state1') = step1 state1 a in 
                (b, (state2', state1'))), 
          (state2, state1)) 
    in 
    fun (SF (step1, state1)) -> 
      fun (Str (Co (step2, state2))) -> 
        let (h',s') = aux (step1, state1) (step2, state2)  in Str (Co (h',s'))

(** Convert stream to synchronous function *)

let sf_of_stream : 'a stream -> ('b,'a) sf = 
  fun s -> 
    loop (arr (fun (_, s) -> 
              (head s, tail s))) s

type 'a event = 'a option

(* let switch : ('a, 'b  * ('c event)) sf -> ('c -> ('a, 'b) sf) -> ('a, 'b) sf *)
