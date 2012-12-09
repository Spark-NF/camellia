(* Initialization *)
let _ = Sdlt.sdl_init ()
let _ = GMain.init ()

(* Useful functions *)
let may f x =
	match x with
		| None -> ()
		| Some x -> let _ = f x in ()

(* Main window *)
let window =
	let wnd = GWindow.window 
		~position:`CENTER
		~title:"Camellia" () in
	wnd#connect#destroy GMain.quit;
	wnd

(* Layout *)
let vbox = GPack.vbox 
	~spacing:5
	~border_width:5
	~packing:window#add ()

(* Image *)
let result_image =
	let scroll = GBin.scrolled_window
		~height:200
		~width:200
		~packing:vbox#add () in
	let img = GMisc.image
		~packing:scroll#add_with_viewport () in
	img

(* OCR *)
let analyze_file file =
	let img = Binarize.binarize file in
	let angle = Skew.skew img in
	let angle2 =
		if angle > Sdlt._PI /. 4.0 then
			Sdlt._PI /. 2.0 -. angle
		else if angle < Sdlt._PI /. (-4.0) then
			angle +. Sdlt._PI /. 2.0
		else
			angle in
	let img = Skew.rotate img angle2 in
	begin
		let final_result = Cutter.xy_cut img in
		let final_image = Cutter.draw_rects [final_result] img (100, 100, 100) in
		begin
			Sdlvideo.save_BMP final_result "~tmp.bmp";
			result_image#set_file "~tmp.bmp"
		end
	end

(* Buttons *)
let bbox = GPack.button_box `HORIZONTAL
	~spacing:5
	~layout:`SPREAD
	~packing:(vbox#pack ~expand:false) ()

(* Button "Open" *)
let open_button =
	let dlg = GWindow.file_chooser_dialog
		~action:`OPEN
		~parent:window
		~position:`CENTER_ON_PARENT
		~destroy_with_parent:true () in
	dlg#add_button_stock `CANCEL `CANCEL;
	dlg#add_select_button_stock `OPEN `OPEN;
	let btn = GButton.button
		~stock:`OPEN
		~packing:bbox#add () in
			GMisc.image
				~stock:`OPEN
				~packing:btn#set_image ();
	btn#connect#clicked (fun () ->
		result_image#set_file "loading.gif";
		if dlg#run () == `OPEN then
			begin
				dlg#misc#hide_all ();
				may analyze_file dlg#filename
			end
		else
			result_image#clear ());
	btn

(* Button "About" *)
let about_button =
	let dlg = GWindow.about_dialog
		~authors:["Spark-NF (<nicolas.faure.7@gmail.com>)";
			"babiboubu (<eassaez@gmail.com>)";
			"Judas (<sebastien.trouve.1993@gmail.com>)";
			"Noxenn (<alexandre.urion@gmail.com>)"]
		~copyright:"Gentlemen Coders"
		~license:"Apache License 2.0"
		~version:"2.0"
		~website:"http://www.camellia.fr.cr/"
		~website_label:"Camellia"
		~position:`CENTER_ON_PARENT
		~parent:window
		~destroy_with_parent:true () in
	let btn = GButton.button
		~stock:`ABOUT
		~packing:bbox#add () in
			GMisc.image
				~stock:`ABOUT
				~packing:btn#set_image ();
	btn#connect#clicked (fun () -> ignore (dlg#run ()); dlg#misc#hide ());
	btn

(* Button "Quit" *)
let quit_button = 
	let btn = GButton.button
		~stock:`QUIT
		~packing:bbox#add () in
			GMisc.image
				~stock:`QUIT
				~packing:btn#set_image ();
	btn#connect#clicked ~callback:GMain.quit;
	btn

(* The end *)
let _ =
	if Array.length Sys.argv > 1 then
		analyze_file Sys.argv.(1);
	window#show ();
	GMain.main ()
