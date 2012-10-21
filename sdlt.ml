let _PI = 3.14159265359 ;;

let abs a = if a > 0.0 then a else -. a ;;



let sdl_init () =
	begin
		Sdl.init [`EVERYTHING];
		Sdlevent.enable_events Sdlevent.all_events_mask;
	end ;;

let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w,
	 (Sdlvideo.surface_info img).Sdlvideo.h) ;;

let get_pixel img x y =
	if Sdlvideo.get_pixel_color img x y = (0, 0, 0) then 1 else 0 ;;



let show img dst =
	let d = Sdlvideo.display_format img in
	Sdlvideo.blit_surface d dst ();
	Sdlvideo.flip dst ;;

let rec wait_key () =
	let e = Sdlevent.wait_event () in
	match e with
		  Sdlevent.KEYDOWN _ -> ()
		| _ -> wait_key () ;;
