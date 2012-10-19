(* Trucs et choses *)

let _PIXEL_COUNT = 2 ;;
let _PI = 3.14159265359 ;;



(* Détecte l'angle de l'image *)

let get_pixel img x y =
	Sdlvideo.get_pixel_color img x y = (0, 0, 0) ;;

let skew img =
	let (w, h) = get_dimensions img in
	let first_left = ref (-1, -1) in
	let first_top = ref (-1, -1) in
	begin
		let x = ref 0 in
		while (!first_left = (-1, -1)) && (!x <> w) do
			let temp = ref 0 in
			let first_pixel = ref (-1, -1) in
			for y = 0 to h-1 do
				temp := !temp + (get_pixel img x y);
				if !first_pixel = (-1, -1) then
					first_pixel := (!x, y)
			done;
			if !temp >= _PIXEL_COUNT then
				first_left := !first_pixel;
			x := !x +1
		done;
		let y = ref 0 in
		while (!first_top = (-1, -1)) && (!y <> h) do
			let temp = ref 0 in
			let first_pixel = ref (-1, -1) in
			for x = 0 to w-1 do
				temp := !temp + (get_pixel img x y);
				if !first_pixel = (-1, -1) then
					first_pixel := (x, !y)
			done;
			if !temp >= _PIXEL_COUNT then
				first_top := !first_pixel;
			y := !y +1
		done;
		if (!first_left <> (-1, -1)) && (!first_top <> (-1, -1)) then
			let (x1, y1) = !first_left in
			let (x2, y2) = !first_top in
			let dx = float_of_int (x2 - x1) in
			let dy = float_of_int (y2 - y1) in
			atan (dy /. dx)
		else
			failwith "could not find image's boundaries for angle detection"
	end ;;



(* Rotate l'image *)

let rotate img angle =
	if angle = 0.0 then
		img
	else
		let (width, height) = get_dimensions img in
		let x_m = width / 2 and y_m = height / 2 in
		let x_d = ref 0 and y_d = ref 0 in
		let dest = Sdlvideo.create_RGB_surface_format img [] width height in
		begin
			for y_s = 0 to height do
				for x_s = 0 to width do
					begin
						y_d :=	int_of_float (((float_of_int (y_s - y_m)) *. (cos angle)) -.
									((float_of_int (x_s - x_m)) *. (sin angle))) + y_m;
						x_d :=	int_of_float (((float_of_int (y_s - y_m)) *. (sin angle)) +.
									((float_of_int (x_s - x_m)) *. (cos angle))) + x_m;
						if (!y_d < height) && (!y_d >= 0) && (!x_d < width) && (!x_d >= 0) then
							Sdlvideo.put_pixel_color dest !x_d !y_d (Sdlvideo.get_pixel_color img x_s y_s);
					end
				done
			done;
			dest
		end ;;