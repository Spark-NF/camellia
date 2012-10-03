print_endline "
************************
*                      *
* The Camellia Project *
*                      *
************************
*                      *
* Authors:             *
*   Faure Nicolas      *
*   Polsinelli Max     *
*   Trouve Sebastien   *
*   Urion Alexandre    *
*                      *
************************\r\n";;

let file_to_matrix file = 
	Array.make_matrix 1 1 0;;

let greyscale matrix = 
	matrix;;

let remove_noise matrix = 
	matrix;;

let rotate matrix = 
	matrix;;

let get_texts matrix = 
	matrix;;

let analyze file =
	begin
		print_endline ("Analyzing file \"" ^ file ^ "\"...");
		let matrix = file_to_matrix file in
		let greyscaled_matrix = greyscale matrix in
		let nonoise_matrix = remove_noise greyscaled_matrix in
		let rotated_matrix = rotate nonoise_matrix in
			get_texts rotated_matrix
	end;;

let main () =
	if Array.length Sys.argv > 1 then
		begin
			let texts = analyze Sys.argv.(1) in
			begin
				print_endline ("Finished analyzing file.");
				print_int texts.(0).(0);
				print_string "\r\n"
			end
		end
	else
		failwith "Error: you must provide a file to the command line.";;
		
let _ = main ();;