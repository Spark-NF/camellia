print_endline "The Camellia Project\r\n";;

let analyse file =
	print_endline ("Opening file \"" ^ file ^ "\"...");
	print_endline "test";
in

if Array.length Sys.argv > 1 then
	analyse Sys.argv.(1)
else
	failwith "Error: you must provide a file.";;