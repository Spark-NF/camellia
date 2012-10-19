(*Fonction qui néttoie les pixels isolés*)

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


(*Fonction de floutage : 
  non utilisé car floutte trop en même temps qu'elle clean*)

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
