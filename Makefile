OCAML		:= ocamlopt
OCAMLFLAGS	:= -I +sdl
OCAMLLD		:= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	sdlt.ml binarize.ml skew.ml cutter.ml main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o camellia sdlt.ml binarize.ml skew.ml cutter.ml main.ml

clean::
	rm -f *~ *.o *.cm? camellia
