open Graphics;;

let width, height = 800, 600;; (* Taille de la fenêtre *)

let gravity = 0.5;; (* Accélération due à la gravité *)
let elasticity = 0.8;; (* Coefficient de restitution *)

let ball_radius = 20;; (* Rayon de la balle *)
let ball_x = width / 2;; (* Position initiale en x *)
let ball_y = height - ball_radius;; (* Position initiale en y *)
let velocity_y = ref 0.0;; (* Vitesse verticale *)

let rec loop ball_y velocity_y =
  clear_graph ();
  
  (* Dessiner la balle *)
  set_color red;
  fill_circle ball_x ball_y ball_radius;

  (* Mettre à jour la position et la vitesse *)
  let new_velocity_y = !velocity_y -. gravity in
  let new_ball_y = ball_y + int_of_float new_velocity_y in

  (* Vérifier les collisions avec le sol *)
  let final_velocity_y, final_ball_y =
    if new_ball_y - ball_radius <= 0 then
      (-.new_velocity_y *. elasticity, ball_radius) (* Rebond *)
    else
      (new_velocity_y, new_ball_y)
  in

  velocity_y := final_velocity_y;
  
  synchronize ();
  Unix.sleepf 0.016; (* Pause pour lisser l'animation *)
  loop final_ball_y velocity_y;;

let () =
  open_graph (Printf.sprintf " %dx%d" width height);
  loop ball_y velocity_y;;
