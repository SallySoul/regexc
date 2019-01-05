# regexc Documentation

These are the modules loaded by the documentation server.

### load.pl

  This is a standard prolog file allows one to use all the libraries with only one statement.

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

### documentation_server.pl

  This file is run by the `doc_server` target in the Makefile.

In addition, there is the `interface.pl`, however this contains the main entry point 
so it is not loaded by the doc server (for now).
