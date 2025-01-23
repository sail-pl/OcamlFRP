open Stream

type ('a,'b) sf = 
  SF : ('a -> ('b * ('a,'b) sf)) -> ('a,'b) sf

type ('a,'b) it =
  IT : ('s -> 'a -> ('b * 's)) * 's -> ('a,'b) it      

let sf_of_it : ('a, 'b) it -> ('a, 'b) sf =
  let rec aux f s = 
    SF (fun a -> 
      let (b, s') = f s a in (b, aux f s'))  
    in fun (IT (f, s)) -> aux f s

let it_of_sf : ('a,'b) sf -> ('a,'b) it = 
  fun f -> 
    IT ((fun (SF g) -> g), f)

let eval_it : ('a, 'b) it -> 'a stream -> 'b stream = 
  fun 
    (IT (f, s1)) 
    (Str (Co (h, s2))) ->
      Str (Co 
        (
          (fun (s1,s2) -> 
            let (a, s2') = h s2 in 
            let (b, s1') = f s1 a in 
            (b, (s1', s2'))),
            (s1, s2)
        ))
(*    
    let sf_of_it (type a b) : (a, b) it -> (a, b) sf =
      let rec aux f s = 
        SF (fun a -> 
          let (b, s') = f s a in (b, aux f s'))  
        in fun (IT (f, s)) -> aux f s
    
  let it_of_sf (type a b) :  (a, b) sf -> (a, b) it = 
    fun f -> IT ((fun (SF g) -> g), f) *)
  

(*
    GOOD ONE, but too much boxing 
  let rec it_of_sf : ('a,'b) it -> ('a,'b) sf =
    fun (IT (f, s)) -> 
      SF (fun a -> 
            let (b, s') = f s a in (b, it_of_sf (IT (f, s')))) *)
    

(* let rec toto 's.(f : 's -> 'a -> ('b * 's)) (s : 's) = 
  fun a -> let (b, s') = f s a in (b, toto f s') *)

(* let rocky3 (IT (f,s)) = SF (rocky f s) *)
(* fonction qui rappelle le coiterateur avec le nouvel etat *)

(* let it_of_sf : ('a, 'b) sf -> ('a, 'b) it =
  fun (SF f) ->
    IT ((fun x -> x), f)  *)

    (* let rec it_of_sf : 
  'a 'b 's. ('s -> 'a -> ('b * 's)) -> 's -> 'a -> ('b * ('a,'b) sf) =
  fun f s a -> 
    let (b, s') = f s a in (b, SF (it_of_sf f s')) *)

(* let rec it_of_sf : ('s -> 'a -> ('b * 's)) -> 's -> ('a,'b) sf =
    fun f s -> 
      SF (fun a -> 
        let (b, s') = f s a in (b, it_of_sf f s'))  *)

(* let rec it_of_sf : 
  'a 'b 's. ('s -> 'a -> ('b * 's)) -> 's -> ('a,'b) sf =
  fun f s -> 
    SF (fun a -> 
      let (b, s') = f s a in (b, it_of_sf f s')) 

let it_of_sf2 : ('a,'b) it -> ('a,'b) sf =
  fun (IT (f, s)) -> it_of_sf f s *)
