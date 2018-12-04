:- consult('load.pl').
:- use_module(library(optparse)).
:- initialization(main, main).

regexc_app_spec(App_Spec) :-
  App_Spec = [
    [ 
      opt(input), 
      type(term), 
      shortflags([i]), longflags(['input']),
      help(['regular expresion to compile'])
    ],
    [ 
      opt(ast_dot), 
      type(term),
      longflags(['ast-dot']),
      help(['Save the dot representation of the AST, to the specified path'])
    ],
    [
      opt(ast_nfa),
      type(term),
      longflags(['ast-nfa']),
      help(['Save the dot representation of the NFA to the specified path'])
    ]
  ].

main(Args) :-
  regexc_app_spec(App_Spec),
  opt_parse(App_Spec, Args, Opts, Positional_Args, []),
  opt_help(App_Spec, Help_Message),
  write(Help_Message),
  write(Positional_Args).

  

