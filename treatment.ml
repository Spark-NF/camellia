(* Type quadra_tree *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * (quadra_tree list) ;;

(* Convertit un Sdlvideo.rect en histogramme de lettre *)
let is_pixel_black x y img =
	if (Sdlvideo.get_pixel_color img x y = (0, 0, 0)) then 1 else 0;;
let rect_to_hist img =
	let hist = ref [] and (w, h) = Sdlt.get_dims img in
		begin
			for y = h downto 1 do
				hist := (is_pixel_black w h img)::(!hist)
			done;
			hist;
		end;;
			
(* Associe l'histogramme à un caractère *)
let hist_to_char hist dico =
	let accuracy = ref 2 in(* marge d'erreur de l'histogramme *)
	let rec compare_hist_letter current_hist letter_hist(* du dico *) =
		 match current_hist with
			| []     -> failwith "No letter to match w/ database"
			| e1::l1 -> match letter_hist with
				| []     -> failwith "No database to match w/ letter"
				| e2::l2 -> if (e1 <= e2+accuracy) & (e1 >= e2-accuracy) then
					compare_hist_letter l1 l2 (* car la première ligne match *)
							else (* Aller à la prochaine lettre du dico *)
								(**** FIX ME PLEASE ****) in
	compare_hist_letter hist (* Première lettre du dico *) (**** FIX ME PLEASE ****);;

(* Convertit une liste d'arbres en liste récursive de listes de caractères *)
let rec_build_mat dico l = function
	| [] -> ()
	| e::tree_list -> begin
			build_mat dico l e;
			rec_build_mat dico l tree_list;
		end;;

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

(* Renvoie récursivement une liste (lignes du paragraphe) de listes (mots de la ligne)
de listes (lettres du mots) *)
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