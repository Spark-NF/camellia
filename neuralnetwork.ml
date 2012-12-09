(* This will be the neuronal network module *)

(* open Hashtbl *) (* Uncomment if needed *)

class letter =
	(* This matrix is the letter we are trying to match
		It comes directly from the XY-Cut operation *)
	object
		val mutable x
		val mutable y
		val mat = Hashtbl.create n
		method get_x = x
		method get_y = y
		method get_mat = mat
		(* NEW TYPE NEEDED TO CREATE A BIDIMENSIONAL MATRIX BELOW *)
		(*
		method rec create_pattern hashtab x y =
				for i = 0 to x do
				for j = 0 to y do
				Hashtabl.add hashtab i j
	end
	*)

	
(* takes the boxes' list from XY-cut operation and create a letter list *)
(*
let rec create_list boxes = match boxes with
		[] -> failwith "Internal error : no caracter detected"
	| (x1,x2,y1,y2)::l -> let create_pattern x y = new letter x y in
		(create_pattern (x2-x1) (y2-y1))::(create_list l)
*)


(* associates boxes from XY-cut operation to letter by filling letters with 
0 and 1 and compares two matrices, returns an accuracy value and *)
let matching pat1 pat2 =
	(**** FIX ME ****)
	
	
(* tries to match the given letter pattern with an entity from a database
(an entire font for example) by calling the previous matching function *)
let first_try pattern matching_list =
	match matching_list with
		[] -> failwith "Internal error : no database for matching"
	| e::l -> matching pattern e;
			first_try pattern l

(* this function returns the number of black pixels in a hashtab *)
(* 
let counter hsh =
	let iter = ref 0 in
		for i = Hashtbl.length hsh1 downto 0 do
			Hashtbl.iter(fun i -> iter := !iter + i) hsh;
*)
			
(* hsh1 is the binary matrix of the current letter.
hsh2 is the binary matrix of the letter taken from the database.
this function compares the number of black pixels in each line of the
two matrices *)
let hshtb_comp hsh1 hsh2 =
	(**** FIX ME ****)