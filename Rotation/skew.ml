let get_dimensions img = (10, 10) ;;
let get_pixel img x y = 1 ;;



(* Trucs et choses *)

let _PIXEL_COUNT = 2 ;;



(* Détecte l'angle de l'image *)

let skew img =
	let (w, h) = get_dimensions img in
	let angle = 0.0 in
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

let rotate img =
	let (w, h) = get_dimensions img in
	img ;;