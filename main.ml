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

let _PI = 3.14159265359 ;;
let sdl_init () =
	begin
		Sdl.init [`EVERYTHING];
		Sdlevent.enable_events Sdlevent.all_events_mask;
	end ;;
let show img dst =
	let d = Sdlvideo.display_format img in
	Sdlvideo.blit_surface d dst ();
	Sdlvideo.flip dst ;;
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key () ;;
let abs a = if a > 0.0 then a else -. a ;;

let analyze file =
	begin
		print_endline ("Analyzing file \"" ^ file ^ "\"");
		let img = Binarize.binarize file in
		let angle = Skew.skew img in
		let angle2 =
			if angle > _PI /. 4.0 then
				_PI /. 2.0 -. angle
			else if angle < _PI /. (-4.0) then
				angle +. _PI /. 2.0
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
			sdl_init ();
			let img = analyze Sys.argv.(1) in
			begin
				let (w,h) = Skew.get_dims img in
				let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
				show img display;
				wait_key ();
				print_newline ();
				exit 0;
			end;
		end
	else
		failwith "Error: you must provide a file to the command line." ;;
		
let _ = main () ;;
