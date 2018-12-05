:- consult('load.pl').
:- use_module(library(optparse)).
:- initialization(main, main).

re2b_spec(App_Spec) :-
  App_Spec = [
    [
      opt(regex), type(atom), meta('REGEX'),
      shortflags([r]), longflags(['regex']),
      help('regular expresion to compile')
    ],
    [
      opt(input_file), type(atom), meta('PATH'),
      shortflags([i]), longflags(['input-file']),
      help('File with newline seperated regular expressions')
    ],
    [
      opt(ast_dot), type(atom), meta('PATH'),
      longflags(['ast-dot']),
      help('Save the dot representation of the AST, to the specified path')
    ],
    [
      opt(ast_nfa), type(atom), meta('PATH'),
      longflags(['ast-nfa']),
      help('Save the dot representation of the NFA to the specified path')
    ],
    [
      opt(help), type(boolean), default(false),
      shortflags([h]), longflags(['help']),
      help('Show the usage of the tool')
    ]
  ].

re2b_parse_options(Parse_Options) :-
  Parse_Options = [
    duplicated_flags(keepall)
  ].

%
% If the user passed -h or --help then we want to print the usage and halt
%
show_help_if_needed(Opts, App_Spec) :-
  member(help(true), Opts), !,
  opt_help(App_Spec, Help_Message),
  write(user_output, Help_Message),
  halt(1).

show_help_if_needed(_, _).

check_regexes([]) :-
  writeln("You must pass at least one regex via --regex <regex> or --input-file <path>"),
  halt(2).
check_regexes(Regex_Strings) :-
  writeln("Regex Strings:"),
  writeln(Regex_Strings).

regex_opt_test(regex(X), <) :-
  atom(X), !.
regex_opt_test(regex(_), =) :-
  !.
regex_opt_test(_, >).

process_regexes(Opts, Regex_Strings, Remaining_Opts) :-
  partition(regex_opt_test, Opts, Regex_Opts, _, Remaining_Opts),
  bagof(Regex_Atom, member(regex(Regex_Atom), Regex_Opts), Regex_Atoms),
  maplist(atom_string, Regex_Atoms, Regex_Strings).

%
% Parse Arguments, check their validity, and process them into a tuple
% (Input_Strings, Outputs) where
% Input_Strings is an array of strings representing regular expressions
% Outputs is an array of terms of the form <stage_output>(<array of output paths>).
%
parse_args(Args, Processed_Opts) :-
  re2b_spec(App_Spec),
  re2b_parse_options(Parse_Options),
  opt_parse(App_Spec, Args, Opts, _, Parse_Options),
  show_help_if_needed(Opts, App_Spec),

  % Grab all the raw regex input and turn it into a list of strings
  process_regexes(Opts, Regex_Strings, Remaining_Args),

  % Grab all the
  Processed_Opts = (Regex_Strings, Remaining_Args).


handle_error_found_flag(true) :-
  writeln("Exiting due to above errors"),
  halt(4).
handle_error_found_flag(false).

process_regex_string(Regex_String, (Asts, Error_Flag), ([Ast | Asts], Error_Flag)) :-
  regex:string_ast(Regex_String, Ast, []).

process_regex_string(Regex_String, (Asts, _), (Asts, true)) :-
  regex:string_ast(Regex_String, _, Errors),
  print_errors(Regex_String, Errors).

combine_regex_asts([]) :-
  writeln(user_error, "COMPILER ERROR: No regex Asts to combine"),
  halt(5).

combine_asts([First_Ast | Rest_Of_Asts], Combined_Ast) :-
  foldl(combine_regex_asts_fold, Rest_Of_Asts, First_Ast, Combined_Ast).

combine_asts_fold(Current_Ast, Last_Ast, Next_Ast) :-
  Next_Ast = ast_or(Current_Ast, Last_Ast).

dump_ast_dot(Opts, Ast) :-
  member(ast_dot(Path), Opts), !,
  absolute_file_name(Path, Absolute_Path),
  open(Absolute_Path, write, Ast_Dot_File),
  ast_to_dot(Ast_Dot_File, Ast),
  close(Ast_Dot_File).
dump_ast_dot(_, _).

main(Args) :-
  parse_args(Args, (Regex_Strings, Remaining_Opts)),

  % Parse all the regex strings into Asts, halt if errors are found
  foldl(process_regex_string, Regex_Strings, ([], false), (Asts, Parse_Error_Found_Flag)),
  handle_error_found_flag(Parse_Error_Found_Flag),

  % Combine Asts, dump to file if requested
  combine_asts(Asts, Combined_Ast).
  %  dump_ast_dot(Remaining_Opts, Combined_Ast).




