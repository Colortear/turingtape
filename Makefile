OCB_FLAGS = -use-ocamlfind -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) turing.native

byte: sanity
	$(OCB) turing.byte

profile: sanity
	$(OCB) -tag profile turing.native

debug: sanity
	$(OCB) -tag debug turing.byte

sanity:
	ocamlfind query yojson

test: native
	./turing.native machines/unary_sub.json "111-11="

install_deps:
	@echo "\033[32msearching for dependencies....\033[0m"
	@opam install yojson

.PHONY: all clean byte native profile debug sanity test
