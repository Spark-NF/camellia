OCAML		:= ocamlopt
OCAMLFLAGS	:= -I +sdl
OCAMLLD		:= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	Binarize.ml Skew.ml Cutter.ml main.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o camellia Binarize.ml Skew.ml Cutter.ml main.ml

clean::
	rm -f *~ *.o *.cm? camellia
