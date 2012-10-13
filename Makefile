OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	Binarize.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o camellia main.ml

clean::
	rm -f *~ *.o *.cm? test
