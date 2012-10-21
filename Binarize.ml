(* This is the file that binarizes and cleans the image by deleting the noise *)


let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, 
   (Sdlvideo.surface_info img).Sdlvideo.h)

let level = function
    (r,g,b) -> (0.3 *. float_of_int r +. 
	       0.59 *. float_of_int g +. 
	       0.11 *. float_of_int b) /. 255.

let color2grey c = 
  let x = int_of_float (255. *. level c) in
  (x,x,x)

let image2grey imgRGB imgG w h med min max =
  begin
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
	med:= (!med +. level (Sdlvideo.get_pixel_color imgRGB j i));
	    Sdlvideo.put_pixel_color imgG j i 
	        (color2grey (Sdlvideo.get_pixel_color imgRGB j i));
	let (x,_,_) = (Sdlvideo.get_pixel_color imgRGB j i) in
	if x < !min then min := x
        else if x > !max then max := x;
      done
    done
  end

let resize_spec imgG w h med min max =
  begin
    med := float_of_int(int_of_float(!med)- !min)/. float_of_int !max *. 255.;
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
	let (x,_,_) = (Sdlvideo.get_pixel_color imgG j i) in
	let y = 
	  int_of_float((float_of_int( x - !min)/. float_of_int !max)*. 255.)
	  in
	Sdlvideo.put_pixel_color imgG j i (y,y,y);
      done
    done
  end

let clean_pix img w h =
  begin
    for i = 1 to (h-2) do
      for j = 1 to (w-2) do
	let (x,_,_) = Sdlvideo.get_pixel_color img j i in
	if x = 0 then
	  let (y,_,_) = Sdlvideo.get_pixel_color img (j-1) (i)
	  and (z,_,_) = Sdlvideo.get_pixel_color img (j+1) (i) 
	  and (u,_,_) = Sdlvideo.get_pixel_color img (j) (i-1)
	  and (v,_,_) = Sdlvideo.get_pixel_color img (j) (i+1) in
	    if (u != 0 && v != 0) && (y != 0 && z != 0) then
	      Sdlvideo.put_pixel_color img j i (255,255,255);
      done
    done;
    img; 
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
    for i = 1 to h-2 do
      for j = 1 to w-2 do
	Sdlvideo.put_pixel_color imgC j i (adjustG l j i imgG);
      done
    done;
    imgC;
  end

let grey2black img w h med =
  let (u,_,_) = Sdlvideo.get_pixel_color img 1 1 in
  if u > med then
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x > med - (med/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done
  else
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x < med + ((255-med)/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done

(* Returns the binarized image *)

let binarize arg =
  begin
    let img = Sdlloader.load_image arg in
    let (w,h) = get_dims img in
    let imgG = Sdlvideo.create_RGB_surface_format img [] w h in
    let med = ref 0. 
    and min = ref 0 
    and max = ref 0 in
    let _ = image2grey img imgG w h med min max in
    let median = (int_of_float (!med *. 255.)) / (w*h) in
    let _ = resize_spec imgG w h med min max in
    let _ = grey2black imgG w h median in
    let imgC = clean_pix imgG w h in
    (* let imgC = cleanimg imgC img w h in *)
    imgC
  end
