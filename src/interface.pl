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
      help('Save the dot representation of the AST, to the specified path') ],
    [
      opt(nfa_dot), type(atom), meta('PATH'),
      longflags(['nfa-dot']),
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
  writeln("re2b"),
  writeln("\nSUMMARY:"),
  writeln("Compile regular expressions into binary executables"),
  writeln("\nUSAGE:"),
  writeln("\tre2b [-r <regex> | -i <path] OPTIONS"),
  writeln("\nFLAGS:"),
  write(user_output, Help_Message),
  halt(1).

show_help_if_needed(_, _).

no_regex_string_error :-
  writeln(user_output, "You must pass at least one regex via --regex <regex> or --input-file <path>"),
  halt(2).

regex_opt_test(regex(X), <) :-
  atom(X), !.
regex_opt_test(regex(_), =) :- !.
regex_opt_test(_, >).

process_regexes(Opts, Regex_Strings, Remaining_Opts) :-
  partition(regex_opt_test, Opts, Regex_Opts, _, Remaining_Opts),

  (bagof(Regex_Atom, member(regex(Regex_Atom), Regex_Opts), Regex_Atoms) ;
  no_regex_string_error),

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


handle_error_flag(true, Message) :- !,
  writeln(Message),
  halt(4).
handle_error_flag(false, _Message) :- !.

write_ast_dot(Opts, Ast) :-
  member(ast_dot(Path), Opts), atom(Path), !, 
  absolute_file_name(Path, Absolute_Path),
  open(Absolute_Path, write, Ast_Dot_File),
  regex:ast_to_dot(Ast_Dot_File, Ast),
  close(Ast_Dot_File).
write_ast_dot(_, _).

write_nfa_dot(Opts, Nfa) :-
  member(nfa_dot(Path), Opts), atom(Path), !, 
  absolute_file_name(Path, Absolute_Path),
  open(Absolute_Path, write, Nfa_Dot_File),
  statemachine:nfa_to_dot(Nfa_Dot_File, Nfa),
  close(Nfa_Dot_File).
write_nfa_dot(_, _).

main(Args) :-
  parse_args(Args, (Regex_Strings, Remaining_Opts)),

  % Parse all the regex strings into Asts, 
  % halt if errors are found
  % dump
  regex_parsing:parse_regex_strings(
    user_output,
    Regex_Strings, 
    Ast,
    Parse_Error_Flag
  ),
  write_ast_dot(Remaining_Opts, Ast),

  handle_error_flag(
    Parse_Error_Flag,
    "Exiting due to above Errors..." 
  ),

  % Create NFA from AST, write to file if requested
  statemachine:ast_nfa(Ast, Nfa),
  (write_nfa_dot(Remaining_Opts, Nfa) ;
  handle_error_flag(true, "Could not transform ast into NFA")).



