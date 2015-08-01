#
# Makefile for OCaml Scheme interpreter.
#

# The OCaml compiler.
OCAMLC = opt

# The build command.
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

lexer_test:
	corebuild lexer_test.native
	mv lexer_test.native lexer_test

parser_test: parser.ml parser.mli
	corebuild parser_test.native
	mv parser_test.native parser_test

clean:
	corebuild -clean
	rm -f ./bs ./parser.ml ./parser.mli ./parser.mly ./parser_test ./lexer_test *.native
