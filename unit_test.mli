(*
 * unit_test.mli
 *
 *     Utilities for creating unit tests.
 *
 *     Ian Kuehne, 2105.
 *
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
   val load_test : In_channel.t -> string list

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
