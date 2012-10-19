(*fonction de binarisation. 
  Attention la référence moyenne est toujours présente,
  surtout bien utiliser la même référence que pour color2grey*)

let grey2black img w h moy =
  let (u,_,_) = Sdlvideo.get_pixel_color img 1 1 in
  if u > moy then
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x > moy - (moy/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done
  else
  for i = 0 to h-1 do
    for j = 0 to w-1 do
      let (x,_,_) = Sdlvideo.get_pixel_color img j i in
      if x < moy + ((255-moy)/5) then 
	Sdlvideo.put_pixel_color img j i (255,255,255)
      else 
	Sdlvideo.put_pixel_color img j i (0,0,0)
    done
  done
