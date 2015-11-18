(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bactrian.
 *
 * Bactrian is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Bactrian is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bactrian.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

(**
    Utilities for creating unit tests.
 *)

open Core.Std

(** Functor for creating a command-line unit test. Designed for regression
    testing, where the unit test can easily be conducted by comparing the
    output to some known good output. The functor conducts the tests by
    checking that running 'load_tests' on the test files produces the same
    results as those in 'result_folder'. *)
module Make(M : sig
   (** Function for converting the test file to a list of strings representing
       the output from the unit to be tested. The representation that is
       output should represent all of the transformations done by the unit to
       be tested. *)
   val load_test : string -> string list

   (** Path to the folder containing the input files. *)
   val test_folder      : string

   (** File extension for the input files. *)
   val test_extension   : string

   (** Name of the folder containing correct results from the tests. *)
   val result_folder    : string

   (** Extension for the correct results. *)
   val result_extension : string

   (** List of test names. *)
   val tests : string list

   (** Name of the unit to be tested, used in command-line help options. *)
   val name : string

end) : sig
   (** A command-line application to be run with Command.run. Allows for
       running all tests and summarizing the results, or printing the complete
       results from a single test. *)
   val command : Command.t
end
