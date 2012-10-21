print_endline "
************************
*                      *
* The Camellia Project *
*                      *
************************
*                      *
* Authors:             *
*   FAURE Nicolas      *
*   POLSINELLI Max     *
*   TROUVE Sebastien   *
*   URION Alexandre    *
*                      *
************************\r\n";;

let analyze file =
	begin
		print_endline ("Analyzing file \"" ^ file ^ "\"");
		let img = Binarize.binarize file in
		let angle = Skew.skew img in
		let angle2 =
			if angle > Sdlt._PI /. 4.0 then
				Sdlt._PI /. 2.0 -. angle
			else if angle < Sdlt._PI /. (-4.0) then
				angle +. Sdlt._PI /. 2.0
			else
				angle in
		let img = Skew.rotate img angle2 in
		begin
			print_endline ("Found angle \"" ^ (string_of_float angle) ^ "\"");
			print_endline ("Rotating image by \"" ^ (string_of_float angle2) ^ "\"");
			Cutter.xy_cut img
		end
	end;;

let main () =
	if Array.length Sys.argv > 1 then
		begin
			Sdlt.sdl_init ();
			let img = analyze Sys.argv.(1) in
			begin
				let (w,h) = Sdlt.get_dims img in
				let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
				Sdlt.show img display;
				Sdlt.wait_key ();
				print_newline ();
				exit 0;
			end;
		end
	else
		failwith "Error: you must provide a file to the command line." ;;
		
let _ = main () ;;
