(* Returns a boolean equals to true if the pixel is black *)
let is_pixel_black x y img =
	Sdlvideo.get_pixel_color img x y = (0, 0, 0) ;;

(* Returns the nb of black pixels in the line between the two x bounds *)
let rec x_hist y x1 x2 img = if (x1 = x2) then 0
	else(if (is_pixel_black x1 y img) then 1 else 0)
		+ (x_hist y (x1 + 1) x2 img) ;;

(* Returns the nb of black pixels in the column between the two y bounds *)
let rec y_hist x y1 y2 img = match y1 with
	| _ when y1 = y2 -> 0
	| y -> (if (is_pixel_black x y img) then 1 else 0)
		+ (y_hist x (y + 1) y2 img) ;;

(* Type quadra_tree *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * (quadra_tree list) ;;

(* Returns the limit under which we can consider the line being empty *)
let w_line_limit img =
	let (w, h) = Sdlt.get_dims img in
	(w / 100) ;;
let w_column_limit img =
	let (w, h) = Sdlt.get_dims img in
	(h / 100) ;;

(*
Returns the tree with the following caracteristics :
 - Its key is a quadruplet (x1, x2, y1, y2) representing the box bounds
 - Its sons are boxes formed by horizontal or vertical cuts in the parent box
 - The order (order >= 1) represents the depth of calculation.
	Put order to (-1) for to calculate until no separation is longer possible.
*)
let rec x_cut (x1, x2, y1, y2) tmp_y1 order img =
	if (tmp_y1 = (-1)) then
	(* If not in black strip *)
		(if (y1 = y2 + 1) then
		(* If out of bounds*)
			[]
		else (if ((x_hist y1 x1 x2 img) < w_line_limit img) then
		(* If white line *)
			x_cut (x1, x2, y1 +1, y2) (-1) order img
			else (* (x_hist y1 x1 x2 img> w_line_limit) *)
			(* If black line *)
			x_cut (x1, x2, y1 +1, y2) y1 order img))
	else (* (tmp_y1 <> -1) *)
	(* If in black strip *)
		(if (y1 = y2 + 1) then
		(* If out of bounds *)
			[
				Node(x1, x2, tmp_y1, y1 - 1,
				(if (order > 0)
				then (y_cut (x1, x2, tmp_y1, y1 - 1) (-1) (order - 1) img)
				else []))
			]
		else (if (x_hist y1 x1 x2 img < w_line_limit img) then
		(* If white line *)
			(Node(
				x1, x2, tmp_y1, y1 - 1,
				(if (order > 0)
				then (y_cut (x1, x2, tmp_y1, y1 - 1) (-1) (order - 1) img)
				else [])
			):: (x_cut (x1, x2, y1 +1, y2) (-1) order img))
			else (* (x_hist y1 x1 x2 img > w_line_limit) *)
			(* If black line *)
			(x_cut (x1, x2, y1 +1, y2) tmp_y1 order img)))
and
(* Ycut is Xcut vertical symetric *)
y_cut (x1, x2, y1, y2) tmp_x1 order img =
	if (tmp_x1 = (-1)) then
	(* If not in black strip *)
		(if (x1 = x2 +1) then
		(* If out of bounds*)
			[]
		else (if ((y_hist x1 y1 y2 img) < w_column_limit img) then
		(* If white column *)
			y_cut (x1 +1, x2, y1, y2) (-1) order img
			else (* (y_hist x1 y1 y2 img> w_column_limit) *)
		(* If black column *)
			y_cut (x1 +1, x2, y1, y2) x1 order img))
	else (* (tmp_x1 <> -1) *)
	(* If in black strip *)
		(if (x1 = x2 +1) then
		(* If out of bounds *)
			[
				Node(tmp_x1, x1 -1, y1, y2,
				(if (order > 0)
				then (x_cut (tmp_x1, x1 -1, y1, y2) (-1) (order -1) img)
				else []))
			]
		else (if (y_hist x1 y1 y2 img < w_column_limit img) then
		(* If white column *)
			(Node(
				tmp_x1, x1 -1, y1, y2,
				(if (order > 0)
				then (x_cut (tmp_x1, x1 -1, y1, y2) (-1) (order - 1) img)
				else [])
			):: (y_cut (x1 +1, x2, y1, y2) (-1) order img))
			else (* (y_hist y1 x1 x2 img > w_column_limit) *)
		(* If black column *)
			(y_cut (x1 +1, x2, y1, y2) tmp_x1 order img)))
;;


(* Iterates through tree *)
(* Applies infix and suffix desired functions to all tree nodes *)
let rec it_tree f tree =
	let (in_f, su_f) = f in
	(* Prefix treatment *)
	let tree = in_f tree in
	(* Loop on the sons *)
	let tree = (match tree with
		| Empty -> tree
		| Node(x1, x2, y1, y2, tree_list1) ->
			let rec it_tree_list f tree_list1 = match tree_list1 with
				| [] -> []
				| tree2::tree_list2 ->
					it_tree f tree2 :: it_tree_list f tree_list2 in
			Node(x1, x2, y1, y2, it_tree_list f tree_list1)) in
	(* Suffix treatment *)
	let tree = su_f tree in
	tree
;;


(* Returns tree with merged nodes with unique son *)
let del_son tree = match tree with
		| Empty -> tree
		| Node(x1, x2, y1, y2, tree_list1) ->
			(match tree_list1 with
				| [] -> tree
				| tree2::tree_list2 ->
					(match tree_list2 with
						| [] -> tree2
						| tree3::tree_list2 -> tree))
;;


(* List generic methods *)
let rec get_cell i = function
	| [] -> Empty
	| e::l -> if (i <= 0) then e else get_cell (i-1) l
;;
let rec get_last_cell = function
	| [] -> Empty
	| e::[] -> e
	| _::l -> get_last_cell l
;;

(* Returns tree with borders croped to sons bounds *)
let crop_borders tree = match tree with
		| Empty -> tree
		| Node(_, _, _, _, tree_list) ->
			(match get_cell 0 tree_list with
				| Empty -> tree
				| Node(x1, _, y1, _, _) ->
					(match get_last_cell tree_list with
						| Empty -> tree
						| Node(_, x2, _, y2, _) ->
							Node(x1, x2, y1, y2, tree_list)))
;;


(* Returns tree list average space between lines *)
let rec get_average_y tree_list1 num div = match tree_list1 with
	| [] -> 0
	| Empty :: _ -> 0
	| Node(_, _, _, y2, _) :: tree_list2 ->
		(match tree_list2 with
			| [] -> if (num = 0) then 0 else num / div +1
			| Empty :: _ -> 0
			| Node(_, _, y1, _, _) :: _ ->
				get_average_y tree_list2 (num + y1 - y2) (div +1))
;;
(* Returns tree list average space between columns *)
let rec get_average_x tree_list1 num div = match tree_list1 with
	| [] -> 0
	| Empty :: _ -> 0
	| Node(_, x2, _, _, _) :: tree_list2 ->
		(match tree_list2 with
			| [] -> if (num = 0) then 0 else num / div +1
			| Empty :: _ -> 0
			| Node(x1, _, _, _, _) :: _ ->
				get_average_x tree_list2 (num + x1 - x2) (div +1))
;;

(* Returns tree list with separated lines *)
let rec separate_y tree_list bounds limit left_tree_list =
	match tree_list with
	| [] -> []
	| node1 :: [] -> tree_list
	| node1 :: node2 :: bro_list ->
		(match (node1, node2) with
		| (Empty, _) | (_, Empty) -> tree_list
		| (Node(_, _, _, y2, _), Node(_, _, y1, _, _)) ->
			let (u1, u2, v1, v2) = bounds in
			if (y1 - y2 > limit)
			then
				Node(u1, u2, v1, y2, left_tree_list @ [node1])
				:: separate_y
					(node2 :: bro_list)
					(u1, u2, y1, v2)
					limit
					[node2]
			else
				separate_y
					(node2 :: bro_list)
					(u1, u2, y1, v2)
					limit
					(left_tree_list @ [node2]))
;;
(* Returns tree list with separated columns *)
let rec separate_x tree_list bounds limit left_tree_list =
	match tree_list with
	| [] -> []
	| node1 :: [] -> tree_list
	| node1 :: node2 :: bro_list ->
		(match (node1, node2) with
		| (Empty, _) | (_, Empty) -> tree_list
		| (Node(_, x2, _, _, _), Node(x1, _, _, _, _)) ->
			let (u1, u2, v1, v2) = bounds in
			if (x1 - x2 > limit)
			then
				Node(u1, x2, v1, v2, left_tree_list @ [node1])
				:: separate_x
					(node2 :: bro_list)
					(x1, u2, v1, v2)
					limit
					[node2]
			else
				separate_x
					(node2 :: bro_list)
					(x1, u2, v1, v2)
					limit
					(left_tree_list @ [node2]))
;;

(* Returns tree with separated sons list *)
let separate_xy tree = match tree with
	| Empty -> tree
	| Node(x1, x2, y1, y2, tree_list) ->
			(match tree_list with
			| [] -> tree
			| node1 :: [] -> tree
			| node1 :: node2 :: bro_list ->
				(match (node1, node2) with
				| (Empty, _) | (_, Empty) -> tree
				| (Node(x11, _, _, _, _), Node(x21, _, _, _, _)) when x11 = x21 ->
					let average_y = get_average_y tree_list 0 0 in
					Node(x1, x2, y1, y2,
						separate_y tree_list (x1, x2, y1, y2) average_y [])
				| (Node(_, _, y11, _, _), Node(_, _, y21, _, _)) when y11 = y21 ->
					let average_x = get_average_x tree_list 0 0 in
					Node(x1, x2, y1, y2,
						separate_x tree_list (x1, x2, y1, y2) average_x [])
				| _ -> tree))
;;





(* Draws the 4 lines  *)
let draw_rect (x1, x2, y1, y2) img color =
	begin
		for i = x1 to x2 do
			begin
				Sdlvideo.put_pixel_color img i y1 color;
				Sdlvideo.put_pixel_color img i y2 color;
			end
		done;
		for j = y1 to y2 do
			begin
				Sdlvideo.put_pixel_color img x1 j color;
				Sdlvideo.put_pixel_color img x2 j color;
			end
		done;
		img;
	end
;;


(* Draw the rectangles for each tree node*)
let rec draw_rects tree_list img color =
	match tree_list with
		  Node(x1, x2, y1, y2, tree_list1) :: tree_list2 ->
			let (r, g, b) = color in
			let img = draw_rect (x1, x2, y1, y2) img color in
			let new_color =
				((r +20) mod 255,
				(g + r + 70) mod 255,
				(b + 3 * r) mod 255) in
			let img = draw_rects tree_list1 img new_color in
			draw_rects tree_list2 img color;
		| _ -> img
;;

(* Draw the rectangles for each tree leaf *)
let rec draw_leaf_rects tree_list img =
	match tree_list with
		| Node(x1, x2, y1, y2, []) :: tree_list2 ->
			let color = (Random.int 255, Random.int 255, Random.int 255) in
			let img = draw_rect (x1, x2, y1, y2) img color in
			draw_leaf_rects tree_list2 img;
		| Node(_, _, _, _, tree_list1) :: tree_list2 ->
			let img = draw_leaf_rects tree_list1 img in
			draw_leaf_rects tree_list2 img;
		| _ -> img
;;


(* Entry point *)
let xy_cut img =
	let (w, h) = Sdlt.get_dims img in
	let order = 1 in
	let cuts_tree = Node(0, w, 0, h, x_cut (0, w, 0, h) (-1) order img) in
	let cuts_tree = it_tree (separate_xy, (function x -> x)) cuts_tree in
	let cuts_tree = it_tree (crop_borders, (function x -> x)) cuts_tree in
	draw_rects [cuts_tree] img (100, 100, 100);
;;
