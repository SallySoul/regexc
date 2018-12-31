# regexc

[![Build Status](https://travis-ci.org/SallySoul/regexc.svg?branch=master)](https://travis-ci.org/SallySoul/regexc)

## Introduction

regexc is a regular expression compiler. It takes a regular expression as input and creates a binary,
that matches that regular expression, as output.

My goal for this project was to explore using prolog in a practical setting. That means understanding
things like developer workflow, project layout, and argument parsing. Although the use of the this
project is a bit contrived, I hope that it can serve as an accessable example of for what the source
of a practical prolog application might look like.

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

This project is still under development, and at this stage, the compiler pipeline is not complete.
At the moment regexc can parse regular expressions, and covert the abstract syntax tree into a
Non-Deterministic Automata. Regexc can dump its intermediate representations as dot files. To use dot
files I would recomend installing (graphviz[http://graphviz.org]. Here is a demonstration of how to use
regexc in its current form.

```
regexc -r "\d{2,3}-\d{4,7}-[a-zA-Z_]+" --ast-dot /tmp/ast.dot --nfa-dot /tmp/nfa.dot &&
dot -Tsvg -o /tmp/ast.svg ast.dot &&
dot -Tsvg -o /tmp/nfa.svg nfa.dot &&
open /tmp/ast.svg /tmp/nfa.svg
```

## For Developers

You will need to install [SWI Prolog](http://www.swi-prolog.org). I recomend using your package
manager of choice. I'm on macOS so I used [brew](https://brew.sh).

```
brew install swi-prolog
```

To run the documenation server, use the make target.

```
$ make doc_server
```

This will launch a swipl instance. Use the `halt/0` predicate when you want to stop the server.

While working on the source code, I recomend starting a SWI-Prolog session with that consults the load module.
Whenever you want to consult your changes, run [make/0](http://www.swi-prolog.org/pldoc/doc_for?object=make/0).

```
$ swipl -f src/load.pl
<blah blah>
?-

...

?- make.
% <blah blah>
true.
```

The Makefile has a `test` target, but that includes coverage as well. You can use
the `run_tests/0` goal to run the tests without the extra machinery to find coverage.

```
# Run Tests with the Makefile
$ make test

# Run Coverage tests without coverage results
$ swipl -f src/load.pl -g run_tests
```

To run the main interface without packaging it you can simple load `src/interface.pl`. For example:

```
swipl -f src/interface.pl -- -r "\d{2,3}"
```

If you are adding a new module, please add it to `src/load.pl`.

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

There are unit tests in every module. In addition, there is module that includes integration tests.
For now please run the make target.

```
$ make test
```

## Future Work

* I have not found a good solution for source formatting. There are some promising avenues however.

	- [package-indent](https://github.com/SWI-Prolog/packages-indent/blob/master/indent.pl) is written by
Jan Wielemaker himself. Initially, I found that it did not produce the expected results (removed
lines for example). However I was likely holding it wrong, did not have well made code.

	- [vcs-prolog](https://github.com/arthwang/vsc-prolog) is a VS Code plugin that provides, among
other things, prolog source formatting. It is much newer than package-indent, and was also a supervised
by Jan Wielmaker.

  That being said, I have started to investigate what it would take to create a formatting tool.
Prolog has several use facilities for parsing and serializing code. However these utilities
quickly drop comments, which means that a formatter that takes uncommented predicates is
relativley simple.

* I had trouble with the optparse module. The trouble was not unique to that module, however. The
issue I faced was the the app specification is a very specific kind of structure. Any goals that
expected that structure would simply fail if it did not meet match the expectations. In this case
I think it would be wise to have some goal that can type-check or parse the application structure
and give detailed feedback on why it does not meet expections. Adding such a goal to SWI Prolog might
be worth opening an issue / making a pull request.

	```
	opt_check_spec(+OptsSpec, -Errors)
	```

## Sources

I found this project to be a useful example of many prolog techniques.
[Alan Compiler by thanosqr](https://github.com/thanosqr/Alan-Prolog-Compiler)

I found this SO question useful when investigating how to handle input.
[String Stream in Prolog?](https://stackoverflow.com/questions/23593156/string-stream-in-prolog)

I found this article useful for adding travis support.
[Adding Travis to SWI-Prolog packs](https://rlaanemets.com/post/show/adding-travis-to-swi-prolog-packs)
