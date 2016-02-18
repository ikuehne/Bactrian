Bactrian
========

Copyright Ian Kuehne 2015.

Introduction
------------

Bactrian is an interpreter for a simple Scheme-like language.  The language and
implementation are changing rapidly and primarily intended as an instructive
project, so the language will mostly go undocumented for the moment.

Usage
-----

After cloning the repository, simply type:

$ make

to compile.  OCaml, ocamllex, ocamlyacc, and the Core standard library are
required to build Bactrian.  The resulting executable will be called "bactrian".
It will display a message and prompt when run.  An example session:

$ ./bactrian
Welcome to Bactrian v. 0.0!
=> (define x 10)
->> #u
=> x
->> 10
=> (map - '(1 2 3))
->> (-1 -2 -3)
=> (define (minus-list lst)
           (map - lst))
->> #u
=> (minus-list '(1 2 3))
->> (-1 -2 -3)

"=>" is the default prompt, and "->>" indicates a return value.  '#u' is the
unit value, which is returned from statements that do not produce a value.

Conditions
----------

Bactrian is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

Bactrian is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
Bactrian.  If not, see <http://www.gnu.org/licenses/>.

Author
------

Ian Kuehne
ikuehne@caltech.edu
