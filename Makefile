# 
# Copyright 2015 Ian Kuehne.
#
# Email: ikuehne@caltech.edu
#
# This file is part of Bactrian.
#
# Bactrian is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# Bactrian is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# Bactrian.  If not, see <http://www.gnu.org/licenses/>.
#
 
#
# Makefile for Bactrian.
#

# The OCaml compiler.
OCAMLC = opt

# The build command.
BUILD = corebuild -ocamlopt $(OCAMLC)

# Files to remove.
CLEANUP = bs yacc.ml yacc.mli parser_test lexer_test ast_test *.native *.cmi

# Default compilation.
default: test bs

# Run all unit tests.
test: lexer_test parser_test ast_test
	./lexer_test
	./parser_test
	./ast_test

# Clean the current directory by deleting executables, object files, etc.
clean:
	corebuild -clean
	rm -f $(CLEANUP)

# Simple compilation of interpreter using corebuild.
bs: main.native
	mv main.native bs

main.native: yacc.ml yacc.mli
	$(BUILD) main.native

yacc.ml: yacced_parser
	mv parser/yacc.ml yacc.ml

yacc.mli: yacced_parser
	mv parser/yacc.mli yacc.mli

yacced_parser:
	ocamlyacc parser/yacc.mly

lexer_test: yacc.ml yacc.mli
	corebuild lexer_test.native
	mv lexer_test.native lexer_test

parser_test: yacc.ml yacc.mli
	corebuild parser_test.native
	mv parser_test.native parser_test

ast_test:
	corebuild ast_test.native
	mv ast_test.native ast_test
