(* Type quadra_tree *)
type quadra_tree =
	| Empty
	| Node of int * int * int * int * (quadra_tree list) ;;

(* Convertit un Sdlvideo.rect en histogramme *)
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
			
(* Compare deux histogrammes *)
let rec compare_hist_letter accuracy hist letter =
	 match hist with
		| [] -> true
		| e1::l1 -> match letter with
			| [] -> true
			| e2::l2 ->
				if (e1 <= e2+accuracy) && (e1 >= e2-accuracy) then
					compare_hist_letter accuracy l1 l2
				else
					false ;;

(* Cherche un histogramme dans une liste de paires (lettre, histogramme) *)
let find_hist_letter accuracy x = function
	| [] -> '_'
	| (letter_char, letter_hist)::l ->
		if compare_hist_letter accuracy x letter_hist then
			letter_char
		else
			find_hist_letter accuracy x l ;;

let rect_to_char rect w h dico =
	let hist = rect_to_hist rect in
	let accuracy = ref 1 in (* marge d'erreur de l'histogramme *)
	let format = if (float_of_int h) > (float_of_int h) then "high" else "small" in
	find_hist_letter accuracy hist dico[format] ;;

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
			(rect_to_char letter dico (x2-x1) (y2-y1)) :: l;
		end
	| Node(_, _, _, _, tree_list) -> rec_build_mat dico l tree_list
	| Empty -> ();;

(* Renvoie récursivement une liste de listes des caractères de l'arbre *)
let text_mat tree =
	let lines = [] in
	let dico = Hashtbl.create 2 in
	let dico_high = [] and dico_small = [] in
	begin
		Hashtbl.add dico "high" dico_high;
		Hashtbl.add dico "small" dico_small;
		build_mat dico lines tree;
	end ;;

	
	
			(* METHODE DE ZERNICKE *)

			
(* Implémentation des moments géométriques (en coordonnées cartésiennes)
Prend en paramètre un Sdlvideo.rect et renvoye une liste de moments *)
let geo_mmt img =
	let counter_x = ref 1. and counter_y = ref 1. in
	let moments = ref [] and (w, h) = ref (Sdlt.get_dims img) in
		let rec return_mmt img (w, h) = match (w, h) with
			| (1, 1) | (0, 0) 	-> moments
			| (y, x) when y = 1 -> begin
						counter_x = !counter_x +. 1.;
						moments := ((x.**counter_x) + (y.**counter_y)) :: !moments;
						return_mmt img y (x-1);
						end
			| (y, x) -> begin
						counter_x = !counter_x +. 1.;
						counter_y = !counter_y +. 1.;
						moments := ((x.**counter_x) + (y.**counter_y)) :: !moments;
						return_mmt img (y-1) (x-1);
						end;;









