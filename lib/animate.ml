open Arrows

module type Environment = 
  sig 
    (* type of input *)
    type appinput 

    (* type of output *)
    type appoutput
    (* init function *)
    val init : unit -> unit
    val stop : unit -> unit 
    (* used to build the input stream *)
    val input : bool -> appinput 
    (* used to consume the ouput stream*)
    val output : appoutput -> bool  
  end 

(* runtime for executing a sf *)
module Engine (E : Environment)= 
  struct
    let run (f : ('a,'b) sf) (d : float option) = 
        E.init ();
        let s = coiterate (fun () -> (E.input true,())) ()  in 
          tperform (apply f s) E.output d
  end


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