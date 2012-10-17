OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	Binarize.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o binarize Binarize.ml

imgclean: imgclean.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o imgclean imgclean.ml	

clean::
	rm -f *~ *.o *.cm? test
