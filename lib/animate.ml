open Arrows
open Coiterators
open Thread

let animate : 
  (unit -> 'a) -> 
    ('a,'b) sf -> 
      ('b -> unit) -> int -> unit =  
      fun i sf o n ->
      let input : 'a stream = 
        coiterate (fun () -> (i  (), ())) () in 
      let output = apply sf input in
        perform output o n
        
(* let animate : 
  (unit -> 'a) -> 
    ('a,'b) sf -> 
      ('b -> unit) -> int -> float -> unit =  
      fun i sf o n d ->
        let input : 'a stream = 
          coiterate (fun () -> (i  (), ())) () in 
        let output = apply sf input in
          tperform output o n d *)
      

        (* coiterate : 'a 's. ('s -> 'a * 's) -> 's -> 'a strea *)