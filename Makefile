#
# Makefile for OCaml Scheme interpreter.
#

# The OCaml compiler, with as all warnings enabled.
OCAMLC = opt

BUILD = corebuild -ocamlopt $(OCAMLC)

# Default compilation.
default: bs

# Simple compilation of interpreter using corebuild.
bs:
	ocamlyacc parser/parser.mly
	mv parser/parser.ml parser.ml
	mv parser/parser.mli parser.mli
	$(BUILD) main.native
	mv main.native bs

