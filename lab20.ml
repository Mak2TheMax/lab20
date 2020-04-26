type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;

(* show the image *)
let depict img =
  open_graph "";
  resize_window (List.length (List.hd img)) (List.length img);
  (* create an array type from the 2D list *)
  let image_array = Array.of_list (List.map (fun row -> Array.of_list row) img) in
  (* create a color type from a solid gray value *)
  let solid_color value =
    let lvl = int_of_float (255. *. (1. -. value)) in
    rgb lvl lvl lvl
  in
  (* convert grayscale percentages to rgb color *)
  let color_array = Array.map (fun row -> Array.map
                    (fun pixel -> solid_color pixel) row) image_array in
  (* use Graphics.make_image to create an image to display *)
  let array_image = make_image color_array in
  (* draw image *)
  draw_image array_image 0 0;
  Unix.sleep 2;
;;

(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  List.map  (fun row -> List.map (fun v -> if v <= threshold then 0. else 1.)
                                 row) img ;;

(* dither max image -- dithered image *)
let dither img =
  List.map (fun row ->  List.map
                        (fun v -> if v > Random.float 1. then 1. else 0.)
                        row) img ;;

let mona = Monalisa.image ;;
  depict mona ;;

let mona_threshold = threshold mona 0.75 ;;
  depict mona_threshold ;;

let mona_dither = dither mona ;;
  depict mona_dither ;;

