:- module(interface,
  [
		regexc_app_spec/1,
    main/1
  ]).

:- use_module(library(optparse)).
%:- initialization(main, main).

regexc_app_spec(App_Spec) :-
  App_Spec = [
    [ 
      opt(input), type(term), 
      shortflags([i]), longflags(['input']),
      help(['regular expresion to compile'])
    ],
    [ 
      opt(ast_dor), type(string),
      longflags(['ast-dot'])
      help(['Save the dot representation of the AST, to the specofioed file'])
    ]
  ].

main(Args) :-
  regexc_app_spec(App_Spec),
  %opt_parse(App_Spec, Args, Opts, Positional_Args, []),
  opt_help(App_Spec, Help_Message).

  

