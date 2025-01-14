open Arrows
open Coiterators


let animate : 
  (unit -> 'a) -> 
    ('a,'b) sf -> 
      ('b -> unit) -> int -> unit =  
      fun i sf o n ->
      let input : 'a stream = 
        coiterate (fun () -> (i  (), ())) () in 
      let output = apply sf input in
        perform output o n
        

        (* coiterate : 'a 's. ('s -> 'a * 's) -> 's -> 'a strea *)