open Stream
open Arrows 

type 'a reference = 
  | Internal : 'a ref -> 'a reference
  | Input : 'a option ref * (unit -> 'a) -> 'a reference
  | Output : 'a option ref * ('a -> unit) -> 'a reference

type ref_entry = 
  | RefEntry : 'a reference -> ref_entry

let inputs : ref_entry list ref = ref []

let outputs : ref_entry list ref = ref []
  
let init () = 
  inputs := []; 
  outputs := []

let internal_ref (v : 'a) : 'a reference = 
  Internal (ref v) 

let input_ref (consume : unit -> 'a) : 'a reference = 
  let r = Input (ref None, consume)
  in inputs := RefEntry r :: !inputs; r

let output_ref (produce : 'a -> unit) : 'a reference = 
  let r = Output (ref None, produce) in 
    outputs := RefEntry r :: !outputs; r

let get_value (r : 'a reference ) : 'a = 
  match r with 
  | Internal r -> !r
  | Input (rv, _) -> 
      begin match !rv with 
      | Some x -> rv := None; x 
      | _  -> failwith "Input reference not set"
      end
  | Output _ -> failwith "Can't read value of output reference"

let set_value (r : 'a reference) (a : 'a) : unit = 
  match r with
    | Internal r -> r := a
    | Input _ -> failwith "Can't set value of input reference"
    | Output (rv, _) -> 
        begin match !rv with 
          | None -> rv := Some a;
          | _ -> failwith "Output reference already set"
        end 

let set_inputs () = 
  List.iter (fun (RefEntry r) -> 
    match r with 
    | Input (v, consume) -> 
        v := Some (consume ())
    | _ -> failwith "Can't set input reference" 
  ) !inputs

let reset_outputs () = 
  List.iter (fun (RefEntry r) -> 
    match r with 
    | Output (v,produce) -> 
        begin match !v with
        | Some x -> v:= None; produce x
        | _ -> failwith "Output reference not set"
        end
    | _ -> failwith "Can't set input reference" 
  ) !outputs

let get (r : 'a reference) : ('b, ('b * 'a)) sf =
  
  arr (fun x ->  (x, get_value r))

let set (r : 'a reference) : (('b *'a), 'b) sf =
  arr (fun (x, y) -> set_value r y; x)

  let rec run_ref (f : (unit, unit) sf) (d : float option) : unit = 
    set_inputs ();
    let SF (t, s) = f in
      let (_, _) = t s () in
        begin match d with 
        | None -> ()
        | Some t -> Thread.delay t;
        end;
        reset_outputs ();
        run_ref f d


let run f = run_ref f (Some 0.1)

let get_unit : 'a reference -> (unit, 'a) sf = 
  fun r -> get r >>> arr snd
    
let set_unit : 'a reference -> ('a, unit) sf = 
  fun r -> arr (fun x -> ((), x)) >>> set r
        

  let input_of_stream (s : 'a stream) = 
    let r = ref s in 
      input_ref (fun () -> 
        let (a,s') = destr !r in 
          r := s'; a)
  
  let positives = input_of_stream (coiterate ((+) 1) 0)
  
  let loop a f = 
    let r = internal_ref a in get r >>> f >>> set r 
  

    (* let i = produce (fun () -> ((),())) () in
    let _ = consume (lift f i) (fun _ -> true) d in *)
    
    (* run_ref f d *)

