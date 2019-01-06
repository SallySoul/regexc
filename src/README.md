# regexc Documentation

## Infrastructure Files

These files are infrastructure that support the rest of the project, but aren't strictly speaking
part of the shipped code.

### load.pl

  This is a standard prolog file that can be used to initialize the prolog environment.
  in out case this means loading all the regexc modules.

### documentation_server.pl

  This file is run by the `doc_server` target in the Makefile.

## Source Files

  These files are all modules that contain the logic of regexc.

### regexc_ast.pl

  This is where the grammer for relating regular expressions to Abstract syntax trees lives.
  This module exports a string_ast relation and a couple other utilities for deal with ASTs.

### regexc_parsing.pl

  This module has the support utilities for the interface to call regex_ast. That includes
  predicates for printing errors, handling a list of regular expressions, and combining the
  results of parsing them.

### regexc_statemachine.pl

  This module deals with NFAs and DFAs. It also exports the routine for converting ASTs into
  NFAs.

### regexc_ranges.pl

  Input for the transitions in statemachines are stored as character ranges. This module contains predicates
  relating to ranges.

### regexc_utilities.pl

  This module contains some generic helper functions.

### _Other Source Files_

#### interface.pl

  This contains the optparse application as well as the main entry point for the regexc distributable.
  This file should problably get broken up for testability.
