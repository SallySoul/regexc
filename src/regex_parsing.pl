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
  format(Output_Stream, '^~n', []), !.
write_single_arrow(Output_Stream, N) :-
  format(Output_Stream, ' ', []),
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

print_error(Output_Stream, _Input, error(Message)) :-
  format(Output_Stream, 'ERROR: ~s~n', [Message]).

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

test_write_single_arrow(Num, Correct_Arrow) :-
  with_output_to(string(Arrow),
    assertion(write_single_arrow(current_output, Num))
  ),
  assertion(Arrow = Correct_Arrow).

test(write_single_arrow) :-
  Arrows = [
    (
      0,
      "^\n"
    ),
    (
      1,
      " ^\n"
    ),
    (
      5,
      "     ^\n"
    )
  ],
  forall(member((Num, Correct_Arrow), Arrows),
    test_write_single_arrow(Num, Correct_Arrow)
  ).

test_print_error(Error, Correct_Output) :-
  with_output_to(string(Arrow),
    print_error(current_output, "aaaa", Error)
  ),
  assertion(Arrow = Correct_Output).

test(print_error) :-
  Arrows = [
    (
      error("Wut", some(0)),
      "ERROR: Wut at 0\naaaa\n^\n"
    ),
    (
      error("Hold the phone", some(4)),
      "ERROR: Hold the phone at 4\naaaa\n    ^\n"
    ),
    (
      error("No pos"),
      "ERROR: No pos\n"
    )
  ],
  forall(member((Error, Correct_Output), Arrows),
    test_print_error(Error, Correct_Output)
  ).

test_parse_regex_strings(Strings, Correct_Output, Correct_Ast, Correct_Error_Flag) :-
  with_output_to(string(Output),
      parse_regex_strings(
        current_output,
        Strings,
        Ast,
        Error_Flag
      )
  ),
  assertion(Ast = Correct_Ast),
  assertion(Error_Flag = Correct_Error_Flag),
  assertion(Output = Correct_Output).
/*
test(parser_regex_strings) :-
  Inputs = [
    (
      ["a"],
      "",
      ast_char(a),
      false
    ),
    (
      ["a", "b"],
      "",
      ast_or(ast_char(a), ast_char(b)),
      false
    ),
    (
      ["(a", "b"],
      "ERROR: No closing parenthesis at 0\n(a\n^\n",
      ast_char(b),
      true
    ),
    (
      ["a|b", "c?"],
      "",
      ast_or(ast_or(ast_char(a), ast_char(b)), ast_occurance(ast_char(c), none, some(1))),
      false
    ),
    (
      ["t"],
      "ERROR: Could not parse string at 0\nt\n^\nERROR: No strings were parsed successfully\n",
      Unknown,
      true
    ),
    (
      ["t", "a"],
      "ERROR: Could not parse string at 0\nt\n^\n",
      ast_char(a),
      true
    ),
    (
      [],
      "ERROR: No strings were parsed successfully\n",
      Unknown,
      true
    )

  ],
  forall(member((Strings, Output, Ast, Error_Flag), Inputs),
    test_parse_regex_strings(Strings, Output, Ast, Error_Flag)
  ).
*/
:- end_tests(regex_parsing).
