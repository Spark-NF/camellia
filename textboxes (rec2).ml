

(*
let img_size = (100, 100)
let img_size_x = let (x, y) = img_size in x
let img_size_y = let (x, y) = img_size in y
*)

(* Returns a boolean equals to true if the pixel is black *)
let is_pixel_black x y =
	let color = Sdlvideo.get_pixel_color img x y in
		color = (0, 0, 0)
;;

(* Returns the number of black pixels in the line between the two x bounds *)
let rec y_pix_num y x1 x2 = match x1 with
	| x2 -> 0
	| x -> (if (is_pixel_black x y) then 1 else 0)
		+ y_pix_num y (x+1) x2
;;

(* Returns the number of black pixels in the column between the two y bounds *)
let rec x_pix_num x y1 y2 = match y1 with
	| y2 -> 0
	| y -> (if (is_pixel_black x y) then 1 else 0)
		+ x_pix_num x (y+1) y2
;;

(* Returns the a list of quadra-int (x1, x2, y1, y2) formed by horizontal cuts
in the given box *)
let rec x_cut bounds =
	let (x1, x2, y1, y2) = bounds in
	

(*
Returns the tree with the following caracteristics :
 - It's key is a quadra-int (x1, x2, y1, y2) representing the box bounds
 - It's sons are boxes formed by horizontal or vertical cuts in the parent box
*)
let rec xy_cut order T =
	let cuts_list = match order with
		| 0 -> x_cut T.key
		| 1 -> y_cut T.key in
	if not (is_empty cuts_list) then
		match cuts_list with
		| [] -> T
		| n :: l ->
			begin
			T.son[last_son +1] := n
			xy_cut (abs (order -1)) T.son[last_son]
			end
;;
