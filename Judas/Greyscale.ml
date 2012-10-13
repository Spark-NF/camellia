let level = function
    (r,g,b) -> (0.3 *. float_of_int r +. 
	       0.59 *. float_of_int g +. 
	       0.11 *. float_of_int b) /. 255

let color2grey c = 
  let x = int_of_float (255. *. level c) in
  (x,x,x)

let image2grey imgRGB imgG = 
  let w = (Sdlvideo.surface_info imgRGB).Sdlvideo.w
  and h = (Sdlvideo.surface_info imgRGB).Sdlvideo.h in
  for i = 0 to h do
    for j = 0 to w do
      Sdlvideo.put_pixel_color imgG j i 
	(color2grey (Sdlvideo.get_pixel_color imgRGB j i));
    done
  done    

let binarize arg =
  debut
    let img = Sdlloader.load_image arg in
    let imgG = Sdlvideo.create_RGB_surface_format img [] w h in
    let _ = image2grey img imgG in
    imgG
