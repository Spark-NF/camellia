


let is_pixel_black x y =
	let color = Sdlvideo.get_pixel_color img x y in
		color = (0, 0, 0)

(*
let img_size = (100, 100)
let img_size_x = let (x, y) = img_size in x
let img_size_y = let (x, y) = img_size in y
*)

let bound1 = 

(* Compteur de pixels noirs par ligne entre deux bornes*)
let rec x_path y x1 x2 = match x1 with
	| x2 -> 0
	| x -> (if (is_pixel_black x y) then 1 else 0)
		+ x_path y (x+1) x2

(* Compteur de pixels noirs par colonne entre deux bornes*)
let rec y_path x y1 y2 = match y1 with
	| y2 -> 0
	| y -> (if (is_pixel_black x y) then 1 else 0)
		+ x_path x (y+1) y2

(* Parcours de l'image par ligne*)
let rec separate_box_x x1 x2 y1 y2 = function
	let separations = x_path y

