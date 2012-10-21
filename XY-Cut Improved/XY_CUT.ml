

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
let rec x_hist y x1 x2 = match x1 with
	| x2 -> 0
	| x -> (if (is_pixel_black x y) then 1 else 0)
		+ x_hist y (x+1) x2
;;

(* Returns the number of black pixels in the column between the two y bounds *)
let rec y_hist x y1 y2 = match y1 with
	| y2 -> 0
	| y -> (if (is_pixel_black x y) then 1 else 0)
		+ y_hist x (y+1) y2
;;

(* Type tree *)
Type  quadra_tree = Empty | Node of (int, int, int, int) * quadra_tree list
(* Returns the limit under which we can consider the line being empty *)
let w_line_limit = let (w, h) = img_size in w / 100

(*
Returns the tree with the following caracteristics :
 - It's key is a quadruplet (x1, x2, y1, y2) representing the box bounds
 - It's sons are boxes formed by horizontal or vertical cuts in the parent box
*)
(* Version 1 : goes until no separation is possible *)
let x_cut bounds tmp_y1 =
	let (x1, x2, y1, y2) = bounds in
	if (tmp_y1 = -1) then
		if (y1 = y2 +1) then
			[]
		else if (x_hist y x1 x2 < w_line_limit) then
			x_cut (x1, x2, y1, y2) -1
		else (* (x_hist y x1 x2 < w_line_limit) *)
			x_cut (x1, x2, y1, y2) y1
	else (* (tmp_y1 <> -1) *)
		if (y1 = y2 +1) then
			((x1, x2, tmp_y1, y1 -1), (y_cut (x1, x2, tmp_y1, y1 -1) -1))
		else if (x_hist y x1 x2 < w_line_limit) then
			((x1, x2, tmp_y1, y1 -1), (y_cut (x1, x2, tmp_y1, y1 -1) -1)) :: x_cut (x1, x2, y1, y2) -1
		else []
;;
(* Version 2 : With order (order >= 1) *)
let x_cut (x1, x2, y1, y2) tmp_y1 order =
	if (tmp_y1 = -1) then
		if (y1 = y2 +1) then
			[]
		else if (x_hist y x1 x2 < w_line_limit) then
			x_cut (x1, x2, y1, y2) -1
		else (* (x_hist y x1 x2 < w_line_limit) *)
			x_cut (x1, x2, y1, y2) y1
	else (* (tmp_y1 <> -1) *)
		if (y1 = y2 +1) then
			((x1, x2, tmp_y1, y1 -1),
			if order > 0 then (y_cut (x1, x2, tmp_y1, y1 -1) -1) else [])
		else if (x_hist y x1 x2 < w_line_limit) then
			((x1, x2, tmp_y1, y1 -1),
			if order > 0 then (y_cut (x1, x2, tmp_y1, y1 -1) -1) else []) :: x_cut (x1, x2, y1, y2) -1
		else []
;;

let xy_cut =
	let (w, h) = img_size in
	((0, w, 0, h), x_cut (0, w, 0, h))
;;

(*
(* Assembles the separate letters to form words *)
let assemble_to_words = function
	| (_, [] ) -> (* Feuille *)
	| ((x1, x2, y1, y2), e::l ) ->
		let w = x2 - x1 in
		let ((x3, x4, y3, y4), _) = e
		if (
		
;;
*)

