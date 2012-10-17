let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

let level = function
    (r,g,b) -> (0.3 *. float_of_int r +. 
	       0.59 *. float_of_int g +. 
	       0.11 *. float_of_int b) /. 255.

let color2grey c = 
  let x = int_of_float (255. *. level c) in
  (x,x,x)

let image2grey imgRGB imgG w h moy =
  begin
    for i = 0 to h do
      for j = 0 to w do
	moy:= (!moy +. level (Sdlvideo.get_pixel_color imgRGB j i));
	Sdlvideo.put_pixel_color imgG j i 
	   (color2grey (Sdlvideo.get_pixel_color imgRGB j i));
      done
    done
  end

let grey2black img w h moy =
  let (x,_,_) = Sdlvideo.get_pixel_color img 1 1 in
  if x > moy then
  for i = 0 to h do
    for j = 0 to w do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x > moy - (moy/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done
  else
  for i = 0 to h do
    for j = 0 to w do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x < moy + ((255-moy)/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done

let binarize arg =
  begin
    let img = Sdlloader.load_image arg in
    let (w,h) = get_dims img in
    let imgG = Sdlvideo.create_RGB_surface_format img [] w h in
    let moy = ref 0. in
    let _ = image2grey img imgG w h moy in
    let moyenne = (int_of_float (!moy *. 255.)) / (w*h) in
    let _ = grey2black imgG w h moyenne in
    imgG
  end

let clean_pix img w h =
  begin
    for i = 1 to (h-1) do
      for j = 1 to (w-1) do
	let (x,_,_) = Sdlvideo.get_pixel_color img j i in
	if x = 0 then
	  let (y,_,_) = Sdlvideo.get_pixel_color img (j-1) (i)
	  and (z,_,_) = Sdlvideo.get_pixel_color img (j+1) (i) 
	  and (u,_,_) = Sdlvideo.get_pixel_color img (j) (i-1)
	  and (v,_,_) = Sdlvideo.get_pixel_color img (j) (i+1) in
	    if (u != 0 && v != 0) || (y != 0 && z != 0) then
	      Sdlvideo.put_pixel_color img j i (255,255,255);
      done
    done;
    img; 
  end

let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    sdl_init ();
    let img = binarize Sys.argv.(1) in
    let (w,h) = get_dims img in
    let imgC = clean_pix img w h in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show imgC display;
    wait_key ();
    exit 0;
  end

let _ = main ()
