#
# Makefile for OCaml Scheme interpreter.
#

# The OCaml compiler, with as all warnings enabled.
OCAMLC = opt

BUILD = corebuild -ocamlopt $(OCAMLC)

# Default compilation.
default: bs

# Simple compilation of interpreter using corebuild.
bs: main.native
	mv main.native bs

main.native: parser.ml parser.mli
	$(BUILD) main.native

parser.ml: yacced_parser
	mv parser/parser.ml parser.ml

parser.mli: yacced_parser
	mv parser/parser.mli parser.mli

yacced_parser:
	ocamlyacc parser/parser.mly

clean:
	corebuild -clean
	rm -f ./bs ./parser.ml ./parser.mly
