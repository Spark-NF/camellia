(* Returns the image size *)
let img_size = (100, 100)
;;

(* Returns a boolean equals to true if the pixel is black *)
let is_pixel_black x y img =
	let color = Sdlvideo.get_pixel_color img x y in
		color = (0, 0, 0)
;;

(* Returns the nb of black pixels in the line between the two x bounds *)
let rec x_hist y x1 x2 img = if (x1 = x2) then 0
	else(if (is_pixel_black x y img) then 1 else 0)
		+ (x_hist y (x + 1) x2 img)
;;

(* Returns the nb of black pixels in the column between the two y bounds *)
let rec y_hist x y1 y2 img = match y1 with
	| _ when y1 = y2 -> 0
	| y -> (if (is_pixel_black x y img) then 1 else 0)
		+ (y_hist x (y + 1) y2 img)
;;

(* Type quadra_tree *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * quadra_tree list
;;

(* Returns the limit under which we can consider the line being empty *)
let w_line_limit =
	let (w, h) = img_size in
	(w / 100)
;;

let y_cut (x1, x2, y1, y2) tmp_y1 order img =
	img
;;

(*
Returns the tree with the following caracteristics :
 - It's key is a quadruplet (x1, x2, y1, y2) representing the box bounds
 - It's sons are boxes formed by horizontal or vertical cuts in the parent box
 - The order (order >= 1) represents the depth of calculation.
	Put order to (-1) for to calculate until no separation is longer possible.
*)
let rec x_cut (x1, x2, y1, y2) tmp_y1 order img =
	if (tmp_y1 = (-1)) then
	(* If not in black strip *)
		(if (y1 = y2 + 1) then
		(* If out of bounds*)
			[]
		else (if ((x_hist y1 x1 x2 img) < w_line_limit) then
		(* If white line *)
			x_cut (x1, x2, y1 +1, y2) (-1) order img
			else (* (x_hist y1 x1 x2 img> w_line_limit) *)
		(* If black line *)
			x_cut (x1, x2, y1, y2) y1 order img))
	else (* (tmp_y1 <> -1) *)
	(* If in black strip *)
		(if (y1 = y2 + 1) then
		(* If out of bounds *)
			((
				(x1, x2, tmp_y1, y1 - 1),
				(if (order > 0)
				then (y_cut (x1, x2, tmp_y1, y1 - 1) (-1) (order - 1) img)
				else [])
			):: [])
		else (if (x_hist y1 x1 x2 img < w_line_limit) then
		(* If white line *)
			((
				(x1, x2, tmp_y1, y1 - 1),
				(if (order > 0)
				then (y_cut (x1, x2, tmp_y1, y1 - 1) (-1) (order - 1) img)
				else [])
			):: (x_cut (x1, x2, y1, y2) (-1) order img))
			else (* (x_hist y1 x1 x2 img > w_line_limit) *)
		(* If black line *)
			[]))
;;

(* Changes the pixel color *)
let change_pixel_color x y color img =
	Sdlvideo.set_pixel_color img x y color
;;

(* Draws the 4 lines  *)
(* let draw_rect (x1, x2, y1, y2) img =
	begin
	for i = x1 to x2 do
		begin
			Sdlvideo.put_pixel_color img i y1 (255, 0, 0);
			Sdlvideo.put_pixel_color img i y2 (255, 0, 0);
		end
		done
		for j = y1 to y2 do
		begin
			Sdlvideo.put_pixel_color img x1 j (255, 0, 0);
			Sdlvideo.put_pixel_color img x2 j (255, 0, 0);
		end
	done
	img;
	end
;;
*)

let draw_rect (x1, x2, y1, y2) img = img;;

(* Draw the rectangles around each bounds *)
let rec draw_rects tree_list img = 
	if (tree_list <> []) then
		(let (bounds, tree_list1) :: tree_list2 = tree_list in
		begin
			let img = draw_rect bounds img in
			let img = draw_rects tree_list1 img in
			draw_rects tree_list2 img;
		end)
	else (img)
;;

(* Entry point *)
let xy_cut img =
	let (w, h) = img_size in
	let order = 1 in
	draw_rects [(x_cut (0, w, 0, h) (-1) order img)] img
;;