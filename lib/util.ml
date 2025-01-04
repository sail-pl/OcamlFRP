open Format 
let show (pp : formatter -> 'a -> unit) (l : 'a list) : unit =
  Format.fprintf std_formatter "%a\n" 
    (pp_print_list ?pp_sep:(Some pp_print_space) pp) l
