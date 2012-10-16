OCAML				:= ocamlopt
OCAMLFLAGS	:= -I +sdl
OCAMLLD			:= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	skew.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o camellia skew.ml

clean::
	rm -f *~ *.o *.cm? test
