#
# Makefile for ocaml lab 6.
#

# Default c
default:
	ocamlyacc parser/parser.mly
	mv parser/parser.ml parser.ml
	mv parser/parser.mli parser.mli
	corebuild main.native
	mv main.native bs
