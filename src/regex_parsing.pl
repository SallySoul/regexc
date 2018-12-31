:- module(regex_parsing,
  [
    parse_regex_strings/4,
    format_errors/3,
    format_error/3,
    parse_regex_strings/4
  ]
).

:- use_module(regex_ast).

%! format_errors(+Output_Stream:stream, +Input:string, +Errors:list) is det.
%
% This predicate prints out the error list in a nicely formated way
%
% @arg Output_Stream Where to write the formatted errors
% @arg Input The original string being parsed.
% @arg Errors The list of errors to print
format_errors(_, _, []).
format_errors(Output_Stream, Input, Errors) :-
  maplist(format_error(Output_Stream, Input), Errors).

write_single_arrow(Output_Stream, 0) :-
  format(Output_Stream, '^~n', []), !.
write_single_arrow(Output_Stream, N) :-
  format(Output_Stream, ' ', []),
  M is N - 1,
  write_single_arrow(Output_Stream, M).

%! format_error(+Ouput_Stream:stream, +Input:string, +Error:list) is det.
%
% This predicate prints out the error in a nicely formated way
%
% @arg Output_Stream Where to write the formatted error
% @arg Input The original string being parsed.
% @arg Error The list of errors to print
% TODO: We should probably propagate information about where regex came from
format_error(Output_Stream, Input, error(Message, some(Pos))) :-
  format(Output_Stream, 'ERROR: ~s at ~d~n', [Message, Pos]),
  format(Output_Stream, '~w~n', [Input]),
  write_single_arrow(Output_Stream, Pos).

format_error(Output_Stream, _Input, error(Message)) :-
  format(Output_Stream, 'ERROR: ~s~n', [Message]).

%! process_regex_string
%
% This is used by parse_regex_strings to both transform the string into an AST,
% and to handle formatting the errors.
%
process_regex_string(Output_Stream, Regex_String, (Asts, Error_Flag), ([Ast | Asts], New_Error_Flag)) :-
  regex_ast:string_ast(Regex_String, Ast, Errors), !,
  handle_parse_errors(Output_Stream, Regex_String, Errors, Error_Flag, New_Error_Flag).

% If regex_ast:string_ast fails, we should catch that here.
% Note that we don't get an AST here
process_regex_string(Output_Stream, Regex_String, (Asts, Error_Flag), (Asts, New_Error_Flag)) :-
  Errors = [error("Could not parse string", some(0))],
  handle_parse_errors(Output_Stream, Regex_String, Errors, Error_Flag, New_Error_Flag).

%
% Handling Errors means formatting them and keeping track of
% whether we've seen any with a flag
%
handle_parse_errors(_Output_Stream, _Regex_String, [], Error_Flag, Error_Flag).

handle_parse_errors(Output_Stream, Regex_String, Errors, _Error_Flag, true) :-
  format_errors(Output_Stream, Regex_String, Errors).

%
% Once we have a list of ASTS,
% we need at least one
% We need to cominbe them
%
handle_asts(Output_Stream, [], _, _, true) :-
  writeln(Output_Stream, "ERROR: No strings were parsed successfully").

handle_asts(_, Asts, Ast, Error_Flag, Error_Flag) :-
  regex_ast:combined_asts(Asts, Ast).


%! parse_regex_strings(+Output_Stream:stream, +Regex_Strings:list, -Ast, -Error_Found_Flag) is det.
%
% This is the highest level handle for parsing strings.
% It takes in a list of strings,
% transforms them all into one Ast (by OR'ing them together),
% and formats the errors into an output_stream.
%
% @arg Ouput_Stream Where to write any formatted errors
% @arg Regex_Strings The strings to parse as regular expressions
% @arg AST The resulting AST
% @arg Error_Found_Flag Will be true if any errors were found
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

test_format_error(Error, Correct_Output) :-
  with_output_to(string(Arrow),
    format_error(current_output, "aaaa", Error)
  ),
  assertion(Arrow = Correct_Output).

test(format_error) :-
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
    test_format_error(Error, Correct_Output)
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

test(parser_regex_strings) :-

  Inputs = [
    (
      ["a"],
      "",
      ast_range(97, 97),
      false
    ),
    (
      ["a", "b"],
      "",
      ast_or(ast_range(97, 97), ast_range(98, 98)),
      false
    ),
    (
      ["(a", "b"],
      "ERROR: No closing parenthesis at 0\n(a\n^\n",
      ast_or(ast_range(97, 97), ast_range(98, 98)),
      true
    ),
    (
      ["a|b", "c?"],
      "",
      ast_or(ast_or(ast_range(97, 97), ast_range(98, 98)), ast_occurance(ast_range(99, 99), none, some(1))),
      false
    )
  ],
  forall(member((Strings, Output, Ast, Error_Flag), Inputs),
    test_parse_regex_strings(Strings, Output, Ast, Error_Flag)
  ).

:- end_tests(regex_parsing).
