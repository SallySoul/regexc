:- module(regex_ast,
  [
    string_ast/3,
    ast_to_dot/2,
    combined_asts/2
  ]).

:- use_module(util).

/** <module> regex

This module contains the grammars relating to regular expressions.

Its role is to relate strings to (abstract syntax trees, error list) tuples.

@author Sally Soul
@license MIT
*/

%! string_ast(+Regex:string, -Root_Ast_Node, -Errors:list) is semidet.
%! string_ast(-Regex:string, +Root_Ast_Node, -Errors:list) is semidet.
%
%   Relates a strint to root abstract syntax tree term and a list of errors.A
%
%   @arg Regex This is is the string representation of the regular expression
%   @arg Root_Ast_Node This is the ast_* node representation of the regular expression, modulo errors
%   @arg Errors List of errors (reasons) why string, root are not the same.
string_ast(String, Ast, Errors) :-
  % We /* use  */the atom-as-char, let swipl deal with specifics
  % So this takes string, relates it to a list of chars / their positon
  string_chars(String, Chars),
  util:enumeration(Chars, Enumerated_Chars),

  % Here we relate the list of chars to the grammer
  phrase(gram_expr(Ast, Errors), Enumerated_Chars), !.

string_to_gram(String, Goal) :-
  string_chars(String, Chars),
  util:enumeration(Chars, Enumerated_Chars),
  phrase(Goal, Enumerated_Chars).

%
% This is the top level predicate
%
gram_expr(Ast_Node, Errors) --> gram_or(Ast_Node, Errors).

%
% We can combine to expressions with logical OR
%
gram_or(Ast_Node, Errors) --> gram_concat(Ast_Node, Errors).
gram_or(ast_or(Ast_Node1, Ast_Node2), All_Errors) -->
  gram_concat(Ast_Node1, Errors1),
  [('|', _)],
  gram_or(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.
gram_or(ast_or(Ast_Node1, ast_error), All_Errors) -->
  gram_concat(Ast_Node1, Errors),
  [('|', Pos)],
  { append(Errors, [error("Unexpected OR operator", some(Pos))], All_Errors)}.

%
% Two expressions can be concatenated together
%
gram_concat(Ast_Node, Errors) --> gram_occurance(Ast_Node, Errors).
gram_concat(ast_concat(Ast_Node1, Ast_Node2), All_Errors) -->
  gram_occurance(Ast_Node1, Errors1),
  gram_concat(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.


%
% Finite statemachines allow us to specify that a sequence of an expression
% can occur, with no or some finite minumum number of occurances
% and a finite or unbounded length for the sequence.
%
% There is syntactic sugar for the common cases, like * -> {,},
% However we also want to able to specify other ranges like {3,6}
%
gram_occurance(Final_Ast, Final_Errors) -->
  gram_single(Start_Ast, Start_Errors),
  gram_occurance_specifications(Start_Ast, Start_Errors, Final_Ast, Final_Errors).

%
% We may want nested occurance spcifications, ie. a{2}{,2}
% This is where we parse the specifications
%
gram_occurance_specifications(Start_Ast, Start_Errors, Final_Ast, Final_Errors) -->
  gram_occurance_specification(Start_Ast, Start_Errors, Next_Ast, Next_Errors),
  gram_occurance_specifications(Next_Ast, Next_Errors, Final_Ast, Final_Errors).

gram_occurance_specifications(Next_Ast, Next_Errors, Next_Ast, Next_Errors) --> [].

%
% occurance specifications have a few special cases, * + ?
% They are otherwise specified with {Min,Max} or {Count} notation
% TODO, we could clean this up a bit
%
gram_occurance_specification(Sub_Ast, Sub_Errors, ast_occurance(Sub_Ast, none, some(1)), Sub_Errors) -->
  [('?', _)].
gram_occurance_specification(Sub_Ast, Sub_Errors, ast_occurance(Sub_Ast, none, none), Sub_Errors) -->
  [('*', _)].
gram_occurance_specification(Sub_Ast, Sub_Errors, ast_occurance(Sub_Ast, some(1), none), Sub_Errors) -->
  [('+', _)].
gram_occurance_specification(Sub_Ast, Sub_Errors, ast_occurance(Sub_Ast, Min, Max), Final_Errors) -->
  [('{', _)],
  gram_occurance_fields(Min, Max, Field_Errors),
  [('}', _)],
  {
    append(Sub_Errors, Field_Errors, Final_Errors)
  }.

%
% The fields within brackets are parsed here
%
gram_occurance_fields(Min, Max, []) -->
  maybe_integer(Min),
  [(',', _)],
  maybe_integer(Max).

% TODO: Add Error for a{2-3}

gram_occurance_fields(Reps, Reps, []) -->
  maybe_integer(Reps).

%
% In order to parse occurances we need parse integers
%
maybe_integer(some(I))--> integer(I).
maybe_integer(none) --> [].

%
% greedily reads digits, then converts them in to an integer
%
integer(I) -->
  digit(D0),
  digits(D),
  { number_chars(I, [D0|D])}.

%
% Greedily read in a list of digits
%
digits([D|T]) -->
  digit(D), !,
  digits(T).
digits([]) -->
  [].

%
% Accepts any digit char
%
digit(D) -->
  [(D, _)],
  { char_type(D, digit)}.

% TODO: really? I think this can be better?
any_char(C, Pos) -->
  [(C, Pos)].

%
% Match symbols are syntactic sugar for some common ranges:
% \d => [0-9]
% \D => [^0-9]
% \a => [a-z]
% \A => [A-Z]
% TODO, dunno if I like those leter ranges, seems wrong
%
gram_matching_symbol(Ast, []) -->
  [('d', _)],
  {
    char_code('0', Min_Code),
    char_code('9', Max_Code),
    Ast = ast_range(Min_Code, Max_Code)
  }.

gram_matching_symbol(Ast, []) -->
  [('D', _)],
  {
    char_code('0', Min_Code),
    char_code('9', Max_Code),
    Ast = ast_not(ast_range(Min_Code, Max_Code))
  }.

gram_matching_symbol(Ast, []) -->
  [('w', _)],
  {
    char_code('a', Lowercase_Min),
    char_code('z', Lowercase_Max),
    char_code('A', Uppercase_Min),
    char_code('Z', Uppercase_Max),
    char_code('0', Digit_Min),
    char_code('9', Digit_Max),
    char_code('_', Underscore_Code),
    char_code('-', Dash_Code),
    Ast = ast_or(
      ast_range(Lowercase_Min, Lowercase_Max),
      ast_or(
      ast_range(Uppercase_Min, Uppercase_Max),
      ast_or(
      ast_range(Digit_Min, Digit_Max),
      ast_or(
      ast_range(Underscore_Code, Underscore_Code),
      ast_range(Dash_Code, Dash_Code)
    ))))
  }.

gram_matching_symbol(Ast, []) -->
  [('W', _)],
  {
    char_code('a', Lowercase_Min),
    char_code('z', Lowercase_Max),
    char_code('A', Uppercase_Min),
    char_code('Z', Uppercase_Max),
    char_code('0', Digit_Min),
    char_code('9', Digit_Max),
    char_code('_', Underscore_Code),
    char_code('-', Dash_Code),
    Ast = ast_not(
      ast_or(
      ast_range(Lowercase_Min, Lowercase_Max),
      ast_or(
      ast_range(Uppercase_Min, Uppercase_Max),
      ast_or(
      ast_range(Digit_Min, Digit_Max),
      ast_or(
      ast_range(Underscore_Code, Underscore_Code),
      ast_range(Dash_Code, Dash_Code)
    )))))
}.

gram_matching_symbol(Ast, []) -->
  [('s', _)],
  {
    char_code(' ', Space_Code),
    char_code('\t', Tab_Code),
    Ast = ast_or(
      ast_range(Space_Code, Space_Code),
      ast_range(Tab_Code, Tab_Code)
    )
  }.

gram_matching_symbol(Ast, []) -->
  [('s', _)],
  {
    char_code(' ', Space_Code),
    char_code('\t', Tab_Code),
    Ast = ast_not(
      ast_or(
      ast_range(Space_Code, Space_Code),
      ast_range(Tab_Code, Tab_Code)
    ))
  }.
%
% Control symbols are chacters that
% foundational to the syntax of the regexes.
% They must always be escaped with a '\',
% Including in class definitions
%
control_symbols(S) :-
  S = [
    '\\', '(', ')', '[', ']', '-'
  ].

gram_control_symbol(ast_range(Code, Code), []) -->
  [(C, _)],
  {
    control_symbols(S),
    member(C, S),
    char_code(C, Code)
  }.


%
% Operator symbols are like
% Special symbols, howeer they only need to be escaped outside
% class definitions
%
operator_symbols(S) :-
  S = [
    '.', '+', '*', '?', '{', '}', '|'
  ].

gram_operator_symbol(ast_range(Code, Code), []) -->
  [(C, _)],
  {
    operator_symbols(S),
    member(C, S),
    char_code(C, Code)
  }.

%
% Out side of classes, we must '\' escape control and operator symbols
% And we can freely use the matching symbols
%
gram_special_symbol(Ast, Errors) -->
  [('\\', _)],
  (gram_control_symbol(Ast, Errors) ;
   gram_operator_symbol(Ast, Errors) ;
   gram_matching_symbol(Ast, Errors)).

gram_special_symbol(ast_error, Errors) -->
  [('\\', Pos)], !,
  {
    Errors = [some("Unexpected '\\', was not followed by a special symbol", some(Pos))]
  }.

%
% Inside class defintions, for non range specifiers
% we only need to '\' escape special symbols and matching chars
% Since non_range symbols are a super set of range symbols, we
% cut here if '\' is found
%
gram_class_non_range_symbol(Ast, Errors) -->
  [('\\', _)], !, (gram_control_symbol(Ast, Errors) ;
   gram_matching_symbol(Ast, Errors)).

gram_class_non_range_symbol(ast_error, Errors) -->
  [('\\', Pos)],
  {
    Errors = [error("Unexpected '\\', was not followed by a control or matching symbol", some(Pos))]
  }.

gram_class_non_range_symbol(C, Errors) -->
  any_char(C, Pos),
  {
    (C = ']' -> fail; true),
    control_symbols(Controls),
    member(C, Controls),
    Errors = [
      error(
        "Control characters must be in class definitions escaped with a '\' in order to be matched against",
        some(Pos))]
  }.

gram_class_non_range_symbol(ast_range(Code, Code), []) -->
  any_char(C, _),
  {
    (C = ']' -> fail; true),
    char_code(C, Code)
  }.

%
% In class definitions
% range symbols cannot include matching symbols
%
gram_class_range_symbol(Code, Errors) -->
  [('\\', _)],
  gram_control_symbol(ast_range(Code, Code), Errors).

gram_class_range_symbol('\\', Errors) -->
  [('\\', Pos)], !,
  {
    Errors = [some("Unexpected '\\', was not followed by a control symbol", some(Pos))]
  }.

gram_class_range_symbol(C, Errors) -->
  any_char(C, Pos),
  {
    (C = ']' -> fail; true),
    control_symbols(Controls),
    member(C, Controls),
    Errors = [
      error(
      "Control characters must be escaped with a '\' when used to specify a range",
        some(Pos))]
  }.

gram_class_range_symbol(Code, []) -->
  any_char(C, _),
  {
  %    (C = ']' -> fail; true),
    char_code(C, Code)
  }.

%
% Class definitions are [^C], or [C]
% Where C is a sequence of class members
% Note that operator symbols will be taken literally within [...]
% However special characters need to be escaped with a '\..'
%

gram_class_definition(Ast, Errors) -->
  [('[', Start_Pos)],
  maybe_not_class_definition(Start_Pos, Ast, Errors).

maybe_not_class_definition(Start_Pos, ast_not(Ast), Errors) -->
  [('^', _)],
  middle_of_class_definition(Start_Pos, Ast, Errors).

maybe_not_class_definition(Start_Pos, Ast, Errors) -->
  middle_of_class_definition(Start_Pos, Ast, Errors).

middle_of_class_definition(Start_Pos, Ast, Errors) -->
  gram_class_members(Start_Pos, Ast, Member_Errors),
  end_of_class_definition(Start_Pos, End_Errors),
  {
    append(Member_Errors, End_Errors, Errors)
  }.

end_of_class_definition(_Start_Pos, []) --> [(']', _)].
end_of_class_definition(Start_Pos, Errors) -->
  [],
  {
    Errors = [some("No closing bracket for class defintions", some(Start_Pos))]
  }.

%
% A class member is:
% any symbol
% Note that operator symbols will be taken literally within [...]
% However special characters need to be escaped with a '\..'
%
% A range of symbols: a-z
%
gram_class_member(ast_range(Min_Code, Max_Code), All_Errors) -->
  gram_class_range_symbol(Min_Code, Errors_Min),
  [('-', _)],
  gram_class_range_symbol(Max_Code, Errors_Max),
  {
    append(Errors_Min, Errors_Max, All_Errors)
  }.

% TODO: I think I can save work here:
gram_class_member(ast_error, All_Errors) -->
  gram_class_range_symbol(_Min_Code, Errors),
  [('-', Pos)], !,
  {
    append(
      Errors,
      [error(
        "Unexpected '-', should be followed by a symbol, or preceeded by a '\\'",
        some(Pos))],
      All_Errors)
  }.

gram_class_member(Ast, Errors) -->
  gram_class_non_range_symbol(Ast, Errors).

maybe_class_members(First_Ast, First_Errors, Ast, Errors) -->
  gram_class_member(New_Ast, New_Errors),
  {
    Next_Ast = ast_or(First_Ast, New_Ast),
    append(First_Errors, New_Errors, Next_Errors)
  },
  maybe_class_members(Next_Ast, Next_Errors, Ast, Errors).

maybe_class_members(Ast, Errors, Ast, Errors) --> [].

gram_class_members(_Start_Pos, Ast, Errors) -->
  gram_class_member(First_Ast, First_Errors),
  maybe_class_members(First_Ast, First_Errors, Ast, Errors).

gram_class_members(Start_Pos, ast_error, Errors) -->
  [],
  {
    Errors = [some("No members in class defintions", some(Start_Pos))]
  }.

gram_symbol(Ast, Errors) -->
  [('\\', _)],
  (gram_matching_symbol(Ast, Errors) ; gram_control_symbol(Ast, Errors) ; gram_operator_symbol(Ast, Errors)).

gram_symbol(ast_error, Errors) -->
  [('\\', Pos)],
  {
    Errors = [error("Wut '\\'", Pos)]
  }.

% We don't want to vacuum up operator symbols
gram_symbol(ast_range(Code, Code), []) -->
  any_char(C, _),
  {
    operator_symbols(Operators),
    member(C, Operators) -> fail; true,
    char_code(C, Code)
  }.

/*
gram_symbol(ast_error, Errors) -->
  any_char(C, Pos),
  {
    operator_symbols(Operators),
    control_symbols(Controls),
    (member(C, Operators); member(C, Controls)),
    Errors = [error("Control and operator characters must be escaped with a '\' in order to be matched against", some(Pos))]
  }.
*/
% TODO catch error

%gram_single(Ast, Errors) --> gram_range_section(Ast, Errors).
gram_single(Ast, Errors) --> gram_class_definition(Ast, Errors).
gram_single(ast_wildcard, []) --> [ ('.', _) ].
gram_single(Ast_Node, Errors) --> [('(', _)], gram_expr(Ast_Node, Errors), [(')', _)].
gram_single(Ast_Node, All_Errors) -->
  [('(', Pos)],
  gram_expr(Ast_Node, Errors), ! ,
  {
    append(Errors, [error("No closing parenthesis", some(Pos))], All_Errors)
  }.
gram_single(ast_error, All_Errors) -->
  [('(', Pos)], !,
  {
    All_Errors = [error("No closing parenthesis", some(Pos))]
  }.
gram_single(Ast, Errors) --> gram_symbol(Ast, Errors).

ast_to_dot(Ast, Stream) :-
  format(Stream, "digraph AST {~n", []),
  ast_to_dot_r(Stream, Ast, 0, _),
  format(Stream, "}~n", []).

ast_to_dot_r(Stream, ast_wildcard, Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1,
  format(Stream, "\t~d [label=\"~d: wildcard\"];~n", [Current_Index, Current_Index]).

ast_to_dot_r(Stream, ast_char(C), Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1,
  format(Stream, "\t~d [label=\"~d: char(~a)\"];~n", [Current_Index, Current_Index, C]).

ast_to_dot_r(Stream, ast_range(Min_Code, Max_Code), Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1,
  char_code(Min_Char, Min_Code),
  char_code(Max_Char, Max_Code),
  format(
    Stream,
    "\t~d [label=\"~d: range(~a, ~a)\"];~n",
    [Current_Index, Current_Index, Min_Char, Max_Char]
  ).

ast_to_dot_r(Stream, ast_range(Min_Code, Max_Code), Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1,
  char_code(Min, Min_Code), char_code(Max, Max_Code),
  format(Stream, "\t~d [label=\"~d: range(~w - ~w)\"];~n", [Current_Index, Current_Index, Min, Max]).

ast_to_dot_r(Stream, ast_not(Sub_Ast), Current_Index, Next_Index) :-
  Sub_Ast_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast, Sub_Ast_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: not\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_Index]).

ast_to_dot_r(Stream, ast_occurance(Sub_Ast, Min, Max), Current_Index, Next_Index) :-
  Sub_Ast_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast, Sub_Ast_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Occurance, ~w - ~w\"];~n", [Current_Index, Current_Index, Min, Max]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_Index]).

ast_to_dot_r(Stream, ast_concat(Sub_Ast_L, Sub_Ast_R), Current_Index, Next_Index) :-
  Sub_Ast_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast_L, Sub_Ast_L_Index, Sub_Ast_R_Index),
  ast_to_dot_r(Stream, Sub_Ast_R, Sub_Ast_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Concat\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_L_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_R_Index]).

ast_to_dot_r(Stream, ast_or(Sub_Ast_L, Sub_Ast_R), Current_Index, Next_Index) :-
  Sub_Ast_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast_L, Sub_Ast_L_Index, Sub_Ast_R_Index),
  ast_to_dot_r(Stream, Sub_Ast_R, Sub_Ast_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Or\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_L_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_Ast_R_Index]).

%
% Takes a list of Asts, and combines them with the or operator
%
combined_asts([First_Ast | Rest_Of_Asts], Combined_Ast) :-
  foldl(combined_asts_fold, Rest_Of_Asts, First_Ast, Combined_Ast).

combined_asts_fold(Current_Ast, Last_Ast, Next_Ast) :-
  Next_Ast = ast_or(Current_Ast, Last_Ast).



:- begin_tests(regex_ast).

% A correct string has a 1-1 relationship with some Ast
test_correct_string(Correct_String, Ast) :-
  bagof(Possible_Ast, string_ast(Correct_String, Possible_Ast, Errors), Asts),
  % There is only one Ast.
  % TODO: I think maybe there shouldn't be any choice points here?
  % I think pltest has the easy facility to test a deterministic function
  assertion(Asts = [Ast]),

  % There are no errors
  assertion(Errors = []).

% An incorrection sting will have some ast artifact, and a non-emtpy list of errors.
test_incorrect_string(Incorrect_String, Ast, Errors) :-
  bagof(
    (Possible_Ast, Possible_Errors),
    string_ast(Incorrect_String, Possible_Ast, Possible_Errors),
    Outputs
  ),
  % There should still only be one possible interpretation of the input string.
  % I think.
  assertion(Outputs = [(Ast, Errors)]).

test(correct_strings) :-
  Correct_Strings= [
    (
      "a",
      ast_range(97, 97)
    ),
    (
      "b",
      ast_range(98, 98)
    ),
    (
      "😀",
      ast_range(128512, 128512)
    ),
    (
      ".",
      ast_wildcard
    ),
    (
      "a|b",
      ast_or(ast_range(97, 97), ast_range(98, 98))
    ),
    (
      "a*",
      ast_occurance(ast_range(97, 97), none, none)
    ),
    (
      "a?",
      ast_occurance(ast_range(97, 97), none, some(1))
    ),
    (
      "a+",
      ast_occurance(ast_range(97, 97), some(1), none)
    ),
    (
      "ab",
      ast_concat(ast_range(97, 97), ast_range(98, 98))
    ),
    (
      "ef|gh",
      ast_or(ast_concat(ast_range(101, 101), ast_range(102, 102)), ast_concat(ast_range(103, 103), ast_range(104, 104)))
    ),
    (
      "(ab)*",
      ast_occurance(ast_concat(ast_range(97, 97), ast_range(98, 98)), none, none)
    ),
    (
      "a|b|c",
      ast_or(ast_range(97, 97), ast_or(ast_range(98, 98), ast_range(99, 99)))
    ),
    (
      "a{,}",
      ast_occurance(ast_range(97, 97), none, none)
    ),
    (
      "a{1,}",
      ast_occurance(ast_range(97, 97), some(1), none)
    ),
    (
      "a{,1}",
      ast_occurance(ast_range(97, 97), none, some(1))
    ),
    (
      "a{12,18}",
      ast_occurance(ast_range(97, 97), some(12), some(18))
    ),
    (
      "[ab]",
      ast_or(ast_range(97, 97), ast_range(98, 98))
    ),
    (
      "[a-zA-Z0-9]",
      ast_or(ast_or(ast_range(97, 122), ast_range(65, 90)), ast_range(48, 57))
    ),
    (
      "\\d",
      ast_range(48, 57)
    ),
    (
      "[a-z\\d]",
      ast_or(ast_range(97, 122), ast_range(48, 57))
    ),
    (
      "a{2}{,2}",
      ast_occurance(ast_occurance(ast_range(97, 97), some(2), some(2)), none, some(2))
    ),
    (
      "[b]?+*",
      ast_occurance(ast_occurance(ast_occurance(ast_range(98, 98), none, some(1)), some(1), none), none, none)
    ),
    (
      "(s\\ds)?",
      ast_occurance(ast_concat(ast_range(115, 115), ast_concat(ast_range(48, 57), ast_range(115, 115))), none, some(1))
    ),
    (
      "\\W",
      ast_not(ast_or(ast_range(97, 122), ast_or(ast_range(65, 90), ast_or(ast_range(48, 57), ast_or(ast_range(95, 95), ast_range(45, 45))))))
    )
  ],
  forall(member((Correct_Input, Correct_Output), Correct_Strings),
    assertion(test_correct_string(Correct_Input, Correct_Output))
  ).

test(incorrect_strings) :-
  Incorrect_Strings = [
    (
      "(",
      ast_error,
      [error("No closing parenthesis", some(0))]
    ),
    (
      "(a",
      ast_range(97, 97),
      [error("No closing parenthesis", some(0))]
    ),
    (
      "a|",
      ast_or(ast_range(97, 97), ast_error),
      [error("Unexpected OR operator", some(1))]
    )
  ],
  forall(member((Incorrect_Input, Matching_Ast, Matching_Errors), Incorrect_Strings),
    assertion(test_incorrect_string(Incorrect_Input, Matching_Ast, Matching_Errors))
  ).

test_combined_ast(Asts, Correct_Ast) :-
  combined_asts(Asts, Ast),
  assertion(Ast = Correct_Ast).

test(combined_asts) :-
  Combined_Asts = [
    (
      [ast_char(a)],
      ast_char(a)
    ),
    (
      [ast_char(a), ast_char(b)],
      ast_or(ast_char(b), ast_char(a))
    ),
    (
      [ast_char(a), ast_char(b), ast_char(c)],
      ast_or(ast_char(c), ast_or(ast_char(b), ast_char(a)))
    )
  ],
  forall(member((Asts, Correct_Ast), Combined_Asts),
    assertion(test_combined_ast(Asts, Correct_Ast))
  ).

test_dot_output(String, Correct_Dot_File) :-
  string_ast(String, Ast, Errors),
  assertion(Errors = []),

  tmp_file_stream(text, Test_File, TO), close(TO),
  util:write_to_file(ast_to_dot(Ast), Test_File),
  util:file_diff(Test_File, Correct_Dot_File, Diff),

  string_length(Diff, Diff_Length),
  assertion(Diff_Length = 0),
  (Diff = "" ; format("~n~w~n", [Diff])).

test(ast_to_string) :-
  Dot_Files = [
    (
      "a",
      "tests/ast/ast_1.dot"
    ),
    (
      "a|b|c",
      "tests/ast/ast_2.dot"
    ),
    (
      "(a|b|c){2,3}abc+",
      "tests/ast/ast_3.dot"
    ),
    (
      "a*b+c?(.|(ca)){3,7}",
      "tests/ast/ast_4.dot"
    )
  ],
  forall(member((String, Correct_Dot_File), Dot_Files),
    assertion(test_dot_output(String, Correct_Dot_File))
  ).

:- end_tests(regex_ast).
