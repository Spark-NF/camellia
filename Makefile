OCAML				:= ocamlopt
OCAMLFLAGS	:= -I +sdl
OCAMLLD			:= bigarray.cmxa sdl.cmxa sdlloader.cmxa

tpsdl:	skew.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o camellia Sdl.ml Skew.ml main.ml

clean::
	rm -f *~ *.o *.cm? test
