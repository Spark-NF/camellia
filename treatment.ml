(* Type quadra_tree *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * (quadra_tree list) ;;

(* Convertit une matrice en caractère *)
let matrix_to_char matrix dico = 'c' ;;

(* Convertit une liste d'arbres en liste récursive de listes de caractères *)
let rec_build_mat dico l = function
	| [] -> ()
	| e::tree_list -> begin
			build_mat dico l e;
			rec_build_mat dico l tree_list;
		end ;;

(* Convertit un arbre en liste récursive de listes de caractères *)
let build_mat dico l = function
	| Node(x1, x2, y1, y2, []) ->
		let letter = Sdlvideo.rect x1 x2 y1 y2 in
		begin
			Sdlvideo.save_BMP letter string_of_int(!nb <- nb + 1);
			(matrix_to_char letter dico) :: l;
		end
	| Node(_, _, _, _, tree_list) -> rec_build_mat dico l tree_list
	| Empty -> ();;

(* Renvoie récursivement une liste (lignes du paragraphe) de listes (mots de la ligne) de listes (lettres du mots). *)
let text_mat tree =
	let lines = [] in
	let dico = Hashtbl.create 2 in
	let dico_high = Hashtbl.create 52 
	and dico_small = Hashtbl.create 52 in
	begin
		Hashtbl.add dico "high" dico_high;
		Hashtbl.add dico "small" dico_small;
		build_mat dico lines tree;
	end ;;