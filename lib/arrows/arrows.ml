open Coiterators
open Utils
open Stream   

(* TODO *)
(* use a record in sf type *)
(* functions are themselves iterators, make it explicit *)

(** Arrow type representing length-preserving synchronous functions. *)
type ('a, 'b) sf = 
  SF : ('s -> 'a -> 'b * 's) * 's -> ('a, 'b) sf

(** Creates a pure arrow from a function.*)
let arr : ('a -> 'b) -> ('a, 'b) sf = 
  fun (f : 'a -> 'b) ->
    let step, state = (fun () (a : 'a) -> (f a, ())), () in
      SF (step, state)

(** Apply an arrow to the first component of a product *)
let first : ('a, 'b) sf -> ('a * 'c, 'b * 'c) sf =
  let aux (step, state) = 
    (
      (fun state (a, c) -> 
        let (b, state') = step state a in ((b, c), state')), 
      state )
  in 
    fun (SF (step, state)) -> 
      let (step, state) = aux (step, state) in SF (step, state)

(** Arrow composition *)
let (>>>) : ('a, 'b) sf -> ('b, 'c) sf -> ('a, 'c) sf =
      let aux (step1, state1) (step2, state2) = 
        (fun (state1, state2) a -> 
          let (b, state1') = step1 state1 a in 
            let (c, state2') = step2 state2 b in 
              (c, (state1', state2'))), 
        (state1, state2) in 
    fun (SF (step1, state1)) 
        (SF (step2, state2)) -> 
      let (step, state) = aux (step1, state1) (step2, state2) in 
        SF (step, state)
            

let loop : ('a * 'c, 'b * 'c) sf -> 'c -> ('a, 'b) sf =
  let aux (step, state) c = 
    (fun (state, c) a ->
      let (b,c'), state' = step state (a, c) in  
        (b, (state', c'))), (state, c) 
    in fun (SF (step, state)) c ->
      let (step, state) = aux (step, state) c in 
        SF (step, state)
        
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

        
(** Derived operators *)

(** Apply arrow to second component of product while passing first component unchanged *)
let second  : ('a, 'b) sf -> ('c * 'a, 'c * 'b) sf = 
  fun f -> arr swap >>> first f >>> arr swap

(** Parallel composition of two arrows *)
let parallel : ('a,'b) sf -> ('c, 'd) sf -> ('a * 'c, 'b * 'd) sf = 
  fun f g -> first f >>> second g 

(** Fork input to two arrows and combine outputs *)
let fork : ('a, 'b) sf -> ('a, 'c) sf -> ('a, 'b * 'c) sf  = 
  fun f g -> arr dup >>> parallel f g

(** Convert stream to synchronous function *)
let sf_of_stream : 'a stream -> ('b,'a) sf = 
  fun s -> 
    loop (arr (fun (_, s) -> 
              (head s, tail s))) s
