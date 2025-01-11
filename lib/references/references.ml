open Coiterators

type 'a cell = {content : 'a ref}

let mkref x = {content = ref x}

let get : 'r cell -> ('a, 's) co -> ('a * 'r, 's * unit) co =
  fun r (Co (h,x)) -> 
    Co ((fun (x,()) -> let (a,x') = h x in ((a, !(r.content)), (x',()))) , (x,()))
    
let set : 'r cell -> ('a * 'r, 's) co -> ('a, 's * unit) co = 
  fun r (Co (h, x)) -> 
    Co ((fun (x,()) -> let ((v,a), x') = h x in r.content := v; (a,(x',()))), (x,()))
