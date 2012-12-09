(* Écrit le texte dans un fichier *)
let write txt filename =
	try
		let cout = open_out filename in
		let co = Format.formatter_of_out_channel cout in
		begin
			Format.fprintf co "%s\n" txt;
			close_out cout
		end
	with
		Sys_error _ as e ->
			Format.printf "Can't open \"%s\": %s\n" filename (Printexc.to_string e) ;;

(* Lit le fichier ligne par ligne, renvoyant la liste des lignes *)
let read filename = 
	let lines = ref [] in
	let chan = open_in filename in
	try
		while true; do
			lines := input_line chan :: !lines
		done; []
	with
		End_of_file ->
			close_in chan;
			List.rev !lines ;;