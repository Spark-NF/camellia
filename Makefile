####################

OCAML		:= ocamlopt
OCAMLFLAGS	:= -I +sdl -I +lablgtk2 -I +threads
OCAMLLD		:= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa unix.cmxa threads.cmxa

####################

OUTPUT		:= camellia
SOURCES		:= \
	Sdlt.ml \
	Binarize.ml \
	Skew.ml \
	Cutter.ml \
	main.ml

####################

camellia: ${SOURCES}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${OUTPUT} ${SOURCES}

clean::
	rm -f *~ *.o *.cm? ${OUTPUT} ~tmp.bmp

####################
