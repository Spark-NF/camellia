
(* Rotates the image by a certain angle in radians *)

let rotate img dest w h angle =
	if angle = 0.0 || classify_float angle = FP_nan then
		img
	else
		let x_m = w / 2 and y_m = h / 2 in
		let x_d = ref 0 and y_d = ref 0 in
		begin
			for y_s = 0 to h-1 do
				for x_s = 0 to w-1 do
					Sdlvideo.put_pixel_color dest x_s y_s (255, 255, 255)
				done
			done;
			for y_s = 0 to h-1 do
				for x_s = 0 to w-1 do
					begin
						y_d :=	int_of_float (((float_of_int (y_s - y_m)) *. (cos (-. angle))) -.
									((float_of_int (x_s - x_m)) *. (sin (-. angle)))) + y_m;
						x_d :=	int_of_float (((float_of_int (y_s - y_m)) *. (sin (-. angle))) +.
									((float_of_int (x_s - x_m)) *. (cos(-. angle)))) + x_m;
						if (!y_d < h) && (!y_d >= 0) && (!x_d < w) && (!x_d >= 0) then
							Sdlvideo.put_pixel_color dest x_s y_s (Sdlvideo.get_pixel_color img !x_d !y_d);
					end
				done
			done;
			dest
		end ;;



(* All these functions detect the angle and launch the rotation*)

let histo w h img =
  let a = Array.make w 0 in
  begin
    for i = 0 to (h-1) do
      for j = 0 to (w-1) do
	let (x,_,_) = Sdlvideo.get_pixel_color img j i in
           if (x = 0) then
	     Array.set a i ((Array.get a i) + 1)
      done
    done;
    a
  end

let total w h img = 
  let t = ref 0 in
  begin
    for i = 0 to h-1 do
      for j = 0 to w-1 do
	let (x,_,_) = Sdlvideo.get_pixel_color img j i in
          if x = 0 then
	    t := !t + 1
      done
    done;
    !t
  end

let var a =
  let e = ref 0 
  and e2 = ref 0 in
  begin
    for i = 0 to (Array.length a) - 1 do
      e := !e + Array.get a i;
      e2 := !e2 + ((Array.get a i) * (Array.get a i))
    done;
    e := !e;
    e2 := !e2;
    sqrt (float (!e2 - (!e * !e)));
  end

let test a =
  let n = ref 0 in
  begin
    for i = 0 to (Array.length a) - 1 do
      if Array.get a i > 2 then
	n := !n+1
    done;
    !n
  end
  

let rot img  =
  let image = ref img in
  let n = ref 0. in
  let (w, h) = Sdlt.get_dims !image in
  let size = ref 0 in
  begin
    if w < h then
      size := h + w/2
    else
      size := w + h/2;
    image := Sdlvideo.create_RGB_surface_format img [] !size !size;
    let svg = ref (Sdlvideo.create_RGB_surface_format img [] !size !size) in
    let wb = (!size - w) / 2 in
    let hb = (!size - h) / 2 in
    for i = 0 to !size -1 do
      for j = 0 to !size -1 do
	if i < h+hb && i > hb-1 && j < w+wb && j > wb-1 then
	  Sdlvideo.put_pixel_color !image j i (Sdlvideo.get_pixel_color img (j-wb) (i-hb))
	else
	  Sdlvideo.put_pixel_color !image j i (255,255,255);
      done
    done;
    for i = 0 to !size -1 do
      for j = 0 to !size -1 do
	if i < h+hb && i > hb-1 && j < w+wb && j > wb-1 then
	  Sdlvideo.put_pixel_color !svg j i (Sdlvideo.get_pixel_color img (j-wb) (i-hb))
	else
	  Sdlvideo.put_pixel_color !svg j i (255,255,255)
      done
    done;
    let save = ref !image in
    let dest = Sdlvideo.create_RGB_surface_format img [] !size !size in
    let targ = ref dest in
    let var2 = ref (test (histo !size !size !image)+1) in
    begin
      while (test (histo !size !size !image))  < !var2 do
	var2 := test (histo !size !size !image);
	save := !image;
	image := rotate !image !targ !size !size (-0.01);
	n := !n +. 1.; 
	targ := !save;
      done;
      if !n = 1. then
	while (test (histo !size !size !image))  < !var2 do
	  var2 := test (histo !size !size !image);
	  save := !image;
	  image := rotate !image !targ !size !size (0.01);
	  n := !n -. 1.; 
	  targ := !save;
	done;
      image := rotate !svg !targ !size !size (-. !n /. 100.);
      !image
    end
  end
