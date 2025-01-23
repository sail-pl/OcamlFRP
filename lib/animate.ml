open Stream
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

(* input is evaluated only once !!! No. *)

(* runtime for executing a sf *)
module Engine (E : Environment)= 
  struct
    let run (f : (E.appinput, E.appoutput) sf) (d : float option) = 
        E.init ();
        let s = produce (fun () -> (E.input false,())) ()  in 
          consume (lift f s) E.output d
  end