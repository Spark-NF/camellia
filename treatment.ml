(* This particular module will be used to optimize the XY-Cut and
create a workable structure for the neural network *)

(* open Hashtbl *) (* Uncomment if needed *)

(* Type quadra_tree is here repeated *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * (quadra_tree list) ;;

(* This matrix is carrying the entire text of the page and
 - Each letter is a box of this matrix
 - Words are separated by an empty box
 - A line of this matrix is a paragraph of the text *)
let text_mat () =
	let nb = ref (-1) (* The BMP title will then be used to separate words *)
	and hshtb = Hashtbl.create 1 in
	let build_mat quadra_tree = match quadra_tree with
		| Node(x1, x2, y1, y2, tree_list) ->
			Sdlvideo.save_BMP (Sdlvideo.rect x1 x2 y1 y2) string_of_int(!nb <- nb + 1)
		| _ -> (**** FIX ME ****)
			in (**** FIX ME ****)