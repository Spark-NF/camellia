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

let rec fusion l1 l2 = match (l1,l2) with 
    (l,[]) | ([],l) -> l
   |((a1::l1),(a2::l2)) -> if a1 < a2 then a1::fusion l1 (a2::l2)
                           else a2::fusion (a1::l1 )l2;;

let rec part1 = function
    [] -> []
  | e1::_::l -> e1::part1 l
  | e1::[] -> e1::[];;

let rec part2 = function
    [] -> []
  | _::e2::l -> (e2::part2 l)
  | _::[] -> [];;

let rec lsort = function
    [] -> []
  | e::[] -> e::[]
  | l -> fusion (lsort (part1 l)) (lsort (part2 l));;

let adjustG l w h imgG =
  begin
    l := [];
    for i = h-1 to h+1 do
      for j = w-1 to w+1 do
        l := match Sdlvideo.get_pixel_color imgG j i with
          (x,_,_) -> x :: !l
      done
    done;
    l := lsort !l;
    let x = match (!l) with
      | e1::e2::e3::e4::e5::l -> e5
      | _ -> 0
      in (x,x,x)
  end
   


let cleanimg imgG imgC w h =
  begin
    let l = ref [] in
    for i = 1 to h-1 do
      for j = 1 to w-1 do
	Sdlvideo.put_pixel_color imgC j i (adjustG l j i imgG);
      done
    done;
    imgC;
  end

let grey2black img w h moy =
  let (x,_,_) = Sdlvideo.get_pixel_color img 1 1 in
  if x > moy then
  for i = 0 to h do
    for j = 0 to w do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x > moy - (moy/7) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done
  else
  for i = 0 to h do
    for j = 0 to w do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x < moy + ((255-moy)/7) then 
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
    let imgC = cleanimg imgG img w h in
    let _ = grey2black imgC w h moyenne in
    imgC
  end

let main () =
  begin
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    sdl_init ();
    let img = binarize Sys.argv.(1) in
    let (w,h) = get_dims img in
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    show img display;
    wait_key ();
    exit 0;
  end

let _ = main ()
