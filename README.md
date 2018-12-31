# regexc

regexc is a regular expression compiler. It takes a regular expression as input and creates a binary,
that matches that regular expression, as output.

[![Build Status](https://travis-ci.org/SallySoul/regexc.svg?branch=master)](https://travis-ci.org/SallySoul/regexc)

### Table Of Contents

*  [Introduction](#Introduction)
*  [How To Build](#How-To-Build)
*  [Regular Expression Syntax](#Regular-Expression-Syntax)
*  [For Developers](#For-Developers)
*  [Future Work](#Future-Work)
*  [Helpful Links](#Helpful-Links)

## Introduction

My goal for this project was to explore using prolog in a practical setting. That means understanding
things like developer workflow, project layout, and argument parsing. Although the use of the this
project is a bit contrived, I hope that it can serve as an accessable example for a practical prolog
application might look like.

## How To Build

You will need to install [SWI Prolog](http://www.swi-prolog.org). I recomend using your package
manager of choice. I'm on macOS so I used [brew](https://brew.sh).

```
brew install swi-prolog
```

From there just run `make`. This will create a distributable in `build`.

```
$ make

$ file build/regexc
```

## How To Use Regexc

This project is still under development. At this stage the compiler pipeline is not complete.

At the moment regexc can parse regular expressions, and covert the abstract syntax tree into a
Non-Deterministic Automata. Regexc can dump its intermediate representations as dot files. To use dot
files I would recomend installing [graphviz](http://graphviz.org).

Here is an example of how to use regexc in its current form.

```
regexc -r "\d{2,3}-\d{4,7}-[a-zA-Z_]+" --ast-dot /tmp/ast.dot --nfa-dot /tmp/nfa.dot &&
dot -Tsvg -o /tmp/ast.svg ast.dot &&
dot -Tsvg -o /tmp/nfa.svg nfa.dot &&
open /tmp/ast.svg /tmp/nfa.svg
```

Regexc should provide a useful error message if it fails to parse a regular expression. A generic
error is provided, but I am working towards eliminating the case where it is seen.

```
# An example of a useful error
$ regexc -r "what is (this|that"
ERROR: No closing parenthesis at 8
what is (this|that
        ^
ERROR: No strings were parsed successfully
Exiting due to above Errors...

# The generic (bad) error
$ regexc -r "\d{2-3}"
ERROR: Could not parse string at 0
\d{2-3}
^
ERROR: No strings were parsed successfully
Exiting due to above Errors...
```

## Regular Expression Syntax

Regexc's supported syntax is meant to be similiar to existing implementations.
Broadly speaking here are the supported operations.

```
Expression Grammer (Ignore spaces)

Expr -> Expr Expr                               # Concatenate two expressions
        Expr | Expr                             # Logical OR of two expresions
        Expr *                                  # Expression can occur any number of times
        Expr ?                                  # Expression can occur once or not at all
        Expr +                                  # Expression must occur at least once
        ( Expr )                                # Group the expression
        Expr {Number}                           # Expression must occur Integer number of times
        Expr { Maybe_Number , Maybe_Number }    # Expression Must occur at least the first field
                                                # Expression May not occur more than second field
                                                #   Where Maybe_Number is:
                                                #     Not defined and thus no bound
                                                #       OR
                                                #     Some number bound

Expr -> Single_Char                             # Matches a single character
```

The syntax supports a variety of ways of matching a single character. There is the wildcard, `.` , that can match
any character.

In general, a character in the expression will be taken to literally match that character. Control and
Operator characters must be backslash escaped to be taken literally.

```
Control Symbols   = ['\\', '(', ')', '[', ']', '-']
Operator Symbols  = ['+', '*', '?', '{', '}', '|', '.']
```
"\(" Would then match the '(' instead of being interpreted as starting a group.

There are also classes that can match one of a set of characters. These are specified with `[ Members ]`
notations.

```
Class_Definition -> [ Member Members ]    # Classes must contain at least one member

Members -> Member Members                 #
           []

Member -> Single_Char                     # A single char to include in the class
       -> Single_Char - Single_Char       # A range of characters to include in the class
```

"[a-z\_]" would match any lowercase letter or '\_'.

We also provide some shortcuts to commonly used classes. These class shortcuts can be used freely outside
of class defintions, but can only be used in place of a range when in a class defintions. That is, one
cannot use a class shortcut as a bound in a range.

```
\d -> "[0-9]"             # Any Digit
\D -> "[^0-9]"            # Any non-Digit
\w -> "[a-zA-Z0-9_-]"     # Any "word" character
\W -> "[^a-zA-Z0-9_-]"    # Any non-word character
\s -> "[ \t]"             # Whitespace character
\S -> "[^ \t]"            # Not a whitespace character
```

## For Developers

#### Dependencies

You will need to install [SWI Prolog](http://www.swi-prolog.org). I recomend using your package
manager of choice. I'm on macOS so I used [brew](https://brew.sh).

```
brew install swi-prolog
```

#### Documentation

To run the documenation server, use the `doc_server` target in the Makefile.

```
$ make doc_server
```

This will launch a swipl instance. Use the `halt/0` predicate when you want to stop the server.

#### Workflow

While working on the source code, I recommend starting a SWI-Prolog session that initializes with
the load module. Whenever you want to consult your changes, run
[make/0](http://www.swi-prolog.org/pldoc/doc_for?object=make/0).

```
$ swipl -f src/load.pl
<blah blah>
?-

...

?- make.
% <blah blah>
true.
```

I recommend taking advantage of the top-level (REPL if you prefer), prolog feels really natural
in this environment.
Here is an example where we parse a string and write the AST's dot representation to a file.

```
?- S = "0x[a-f0-9]+".
S = "0x[a-f0-9]+".

?- regex_ast:string_ast($S, AST, Errors).
AST = ast_concat(ast_range(48, 48), ast_concat(ast_range(120, 120), ast_occurance(ast_or(ast_range(97, 102), ast_
range(48, 57)), some(1), none))),
Errors = [],
S = "0x[a-f0-9]+".

```

#### Testing

The Makefile has a `test` target, but that includes a code coverage report as well. You can use
the `run_tests/0` goal to run the tests without the extra machinery to find code coverage.

```
# Run Tests with the Makefile
$ make test

# Run Coverage tests without coverage results
$ swipl -f src/load.pl -g run_tests
```

In addition, the project is currently configured to use [TravisCI](.travis.yml).

#### Using Main Interface

To run the main interface without packaging it you can simply load `src/interface.pl`. Arguments meant
for for the optparse application can be passed after a `--`. For example:

```
swipl -f src/interface.pl -- -r "\d{2,3}"
```

#### Conventions

There are only a few.

* No trailing whitespace.
* Please keep one letter variable names to the absolute minimum. I will accept 'C' as a stand-in for
  'Character' for example. 'C1' and 'C2' are probably not a good idea though. Please strive for descriptive
  variable names.
* If you are adding a new module, please add it to `src/load.pl`. This will, among other things, ensure
  it remains tested.
* Functor and variable names should be composed with '\_'. No Camelcase.

I would like to have an automatic source formatter in the future.

## Design Decisions

### SWI Prolog Only

I am not concerned with compatability for other prolog distributions. While I am always
willing to hear other opinions, for now it seems to me that SWI-Prolog is the only prolog distribtion
with a solid and public ecosystem.

### Argument Parsing
	
SWI Prolog provides an [optparse](http://www.swi-prolog.org/pldoc/man?section=optparse) module that is
modeled after python's own optparse. This module provides the minimum of what I would expect from
an argument parsing library in any language: one specification can be used to generate both a help
message and parse the arguments.

### Testing

SWI Prolog includes a unit test package, [plunit](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)).

There are unit tests in every module. In addition, there will be module for defining integration tests.

## Future Work

* I have not found a good solution for source formatting. There are some promising avenues however.

	- [package-indent](https://github.com/SWI-Prolog/packages-indent/blob/master/indent.pl) is written by
Jan Wielemaker himself. Initially, I found that it did not produce the expected results (removed
lines for example). However I was likely holding it wrong, did not have well made code.

	- [vcs-prolog](https://github.com/arthwang/vsc-prolog) is a VS Code plugin that provides, among
other things, prolog source formatting. It is much newer than package-indent, and was also a supervised
by Jan Wielmaker.

That being said, I have [started to investigate](https://github.com/SallySoul/swi-fmt) what it would take to create a formatting tool.
Prolog has several useful facilities for parsing and formatting code. However these utilities
quickly drop comments, which means that a formatter that takes comments within the predicate
defintion would require far more work. This might be worth it, or it may be wise to avoid this
situation.

* I had trouble with the optparse module. The trouble was not unique to that module, however. The
issue I faced was the the app specification is a very specific kind of structure. Any goals that
expected that structure would simply fail if it did not meet match the expectations. In this case
I think it would be wise to have some goal that can type-check or parse the application structure
and give detailed feedback on why it does not meet expections. Adding such a goal to SWI Prolog might
be worth opening an issue / making a pull request.

	```
	opt_check_spec(+OptsSpec, -Errors)
	```

## Helpful Links

I found this project to be a useful example of many prolog techniques.
[Alan Compiler by thanosqr](https://github.com/thanosqr/Alan-Prolog-Compiler)

I found this SO question useful when investigating how to handle input.
[String Stream in Prolog?](https://stackoverflow.com/questions/23593156/string-stream-in-prolog)

I found this article useful for adding travis support.
[Adding Travis to SWI-Prolog packs](https://rlaanemets.com/post/show/adding-travis-to-swi-prolog-packs)

The regex crate for Rust has a useful set of tools a good documentation for their syntax. I would like
to someday be fully compatable with this specification. Transitivley, this would make regexc compatable
with ripgrep.
[regex-rs syntax documentation](https://docs.rs/regex/1/regex/#syntax)

The author of ripgrep wrote a blog post about text search that is a must-read if you are interested in
that problem space.
[ripgrep is faster than {grep, ag, git grep, ucg, pt, sift}](https://blog.burntsushi.net/ripgrep/)
