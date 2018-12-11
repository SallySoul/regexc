:- module(regex_parsing,
  [
    parse_regex_strings/4
  ]
).

:- use_module(regex_ast).



%! print_errors(+Input:string, +Errors:list) is det. 
%
% This predicate prints out the error list in a nicely formated way
%
% @arg Input The original string being parsed.
% @arg Errors The list of errors to print
print_errors(_, _, []).
print_errors(Output_Stream, Input, Errors) :-
  maplist(print_error(Output_Stream, Input), Errors).

write_single_arrow(Output_Stream, 0) :-
  write(Output_Stream, '^\n'), !.
write_single_arrow(Output_Stream, N) :-
  write(Output_Stream, ' '),
  M is N - 1,
  write_single_arrow(Output_Stream, M).

%! print_errors(+Input:string, +Error:list) is det. 
%
% This predicate prints out the error in a nicely formated way
%
% @arg Input The original string being parsed.
% @arg Error The list of errors to print
% TODO: We should probably propagate information about where regex came from
print_error(Output_Stream, Input, error(Message, some(Pos))) :-
  format(Output_Stream, 'ERROR: ~s at ~d~n', [Message, Pos]),
  format(Output_Stream, '~w~n', [Input]),
  write_single_arrow(Output_Stream, Pos).

% TODO: I think it should be format_error instead of print_error

%! process_regex_string
%
% This is used by parse_regex_strings to both transform the string into an AST, 
% and to handle formatting the errors.
%
process_regex_string(_Output_Stream, Regex_String, (Asts, Error_Flag), ([Ast | Asts], Error_Flag)) :-
  regex_ast:string_ast(Regex_String, Ast, []), !.

% TODO: we should collapse these so that we only call string_ast once
process_regex_string(Output_Stream, Regex_String, (Asts, _), (Asts, true)) :-
  regex_ast:string_ast(Regex_String, _, Errors),
  print_errors(Output_Stream, Regex_String, Errors).

process_regex_string(Output_Stream, Regex_String, (Asts, _), (Asts, true)) :-
  Errors = [error("Could not parse string", some(0))],
  print_errors(Output_Stream, Regex_String, Errors).


handle_asts(Output_Stream, [], _, _, true) :-
  writeln(Output_Stream, "ERROR: No strings were parsed successfully").

handle_asts(_, Asts, Ast, Error_Flag, Error_Flag) :-
  regex_ast:combined_asts(Asts, Ast).


%! parse_regex_strings
%
% This is the highest level handle for parsing strings, it takes in a list of strings
% transforms them all into one Ast (by OR'ing them together),
% and formats the errors into an output_stream.
%
parse_regex_strings(
  Output_Stream, 
  Regex_Strings, 
  Ast, 
  Error_Found_Flag
) :-
  foldl(
    process_regex_string(Output_Stream), 
    Regex_Strings, 
    ([], false), 
    (Asts, Parse_Error_Flag)
  ),

  handle_asts(Output_Stream, Asts, Ast, Parse_Error_Flag, Error_Found_Flag).

:- begin_tests(regex_parsing).

test(print_errors) :- X is 1, X = 1.

:- end_tests(regex_parsing).
