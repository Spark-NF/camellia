(*Les 3 fonctions qui suives servent à mettre en gris 
  et en profite pour calculer la moyenne, le gris min et le gris max. 
  (attention met à jour et utilise 3 reférences)*) 

let level = function
    (r,g,b) -> (0.3 *. float_of_int r +. 
	       0.59 *. float_of_int g +. 
	       0.11 *. float_of_int b) /. 255.

let color2grey c = 
  let x = int_of_float (255. *. level c) in
  (x,x,x)

let image2grey imgRGB imgG w h moy min max =
  begin
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
	moy:= (!moy +. level (Sdlvideo.get_pixel_color imgRGB j i));
	    Sdlvideo.put_pixel_color imgG j i 
	        (color2grey (Sdlvideo.get_pixel_color imgRGB j i));
	let (x,_,_) = (Sdlvideo.get_pixel_color imgRGB j i) in
	if x < !min then min := x
        else if x > !max then max := x;
      done
    done
  end

(*fonction d'élargissement du spectre de gris : 
  augmente la précision de la mis en gris
  (attention utilise 3 références moy min et max)
  (cette fonction ne renvoie rien, 
  elle modifie juste l'image en niveau de gris*)

let resize_spec imgG w h moy min max =
  begin
    moy := float_of_int(int_of_float(!moy)- !min)/. float_of_int !max *. 255.;
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
