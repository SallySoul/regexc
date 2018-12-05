:- consult('load.pl').
:- use_module(library(optparse)).
:- initialization(main, main).

regexc_app_spec(App_Spec) :-
  App_Spec = [
    [ 
      opt(input), type(atom), 
      shortflags([i]), longflags(['input']),
      help('regular expresion to compile')
    ],
    [ 
      opt(ast_dot), type(atom),
      longflags(['ast-dot']),
      help('Save the dot representation of the AST, to the specified path')
    ],
    [
      opt(ast_nfa), type(atom),
      longflags(['ast-nfa']),
      help('Save the dot representation of the NFA to the specified path')
    ],
    [
      opt(help), type(boolean), default(false),
      shortflags([h]), longflags(['help']),
      help('Show the usage of the tool')
    ]
  ].

show_help_if_needed(Opts, App_Spec) :-
  member(help(true), Opts), !,
  opt_help(App_Spec, Help_Message),
  write(user_output, Help_Message),
  halt(1).

show_help_if_needed(_, _).

main(Args) :-
  regexc_app_spec(App_Spec),
  opt_parse(App_Spec, Args, Opts, Positional_Args, []),
  %opt_help(App_Spec, Help_Message),
  %  write(Help_Message),
  show_help_if_needed(Opts, App_Spec),
  
  writeln(Opts),
  writeln(Positional_Args).

  

