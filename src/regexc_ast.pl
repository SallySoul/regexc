:- module(regexc_ast,
  [
    string_ast/3,
    ast_to_dot/2,
    combined_asts/2
  ]).

:- use_module(regexc_utilities).

/** <module> regexc_ast

This module contains the grammars relating to regular expressions.

Its role is to relate strings to (abstract syntax trees, error list) tuples.

@author Sally Soul
@license MIT
*/

%! string_ast(+Regex:string, -AST, -Errors:list) is semidet.
%! string_ast(-Regex:string, +AST, -Errors:list) is semidet.
%
%   Relates a strint to root abstract syntax tree term and a list of errors.A
%
%   @arg Regex This is is the string representation of the regular expression
%   @arg AST This is the ast_* node representation of the regular expression, modulo errors
%   @arg Errors List of errors (reasons) why string, root are not the same.
string_ast(String, AST, Errors) :-
  % We /* use  */the atom-as-char, let swipl deal with specifics
  % So this takes string, relates it to a list of chars / their positon
  string_chars(String, Chars),
  enumeration(Chars, Enumerated_Chars),

  % Here we relate the list of chars to the grammer
  phrase(gram_expr(AST, Errors), Enumerated_Chars), !.

%
% This is the top level predicate
%
gram_expr(AST_Node, Errors) --> gram_or(AST_Node, Errors).

%
% We can combine to expressions with logical OR
%
gram_or(AST_Node, Errors) --> gram_concat(AST_Node, Errors).
gram_or(ast_or(AST_Node1, AST_Node2), All_Errors) -->
  gram_concat(AST_Node1, Errors1),
  [('|', _)],
  gram_or(AST_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.
gram_or(ast_or(AST_Node1, ast_error), All_Errors) -->
  gram_concat(AST_Node1, Errors),
  [('|', Pos)],
  { append(Errors, [error("Unexpected OR operator", some(Pos))], All_Errors)}.

%
% Two expressions can be concatenated together
%
gram_concat(AST_Node, Errors) --> gram_occurance(AST_Node, Errors).
gram_concat(ast_concat(AST_Node1, AST_Node2), All_Errors) -->
  gram_occurance(AST_Node1, Errors1),
  gram_concat(AST_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.


%
% Finite statemachines allow us to specify that a sequence of an expression
% can occur, with no or some finite minumum number of occurances
% and a finite or unbounded length for the sequence.
%
% There is syntactic sugar for the common cases, like * -> {,},
% However we also want to able to specify other ranges like {3,6}
%
gram_occurance(Final_AST, Final_Errors) -->
  gram_single(Start_AST, Start_Errors),
  gram_occurance_specifications(Start_AST, Start_Errors, Final_AST, Final_Errors).

%
% We may want nested occurance spcifications, ie. a{2}{,2}
% This is where we parse the specifications
%
gram_occurance_specifications(Start_AST, Start_Errors, Final_AST, Final_Errors) -->
  gram_occurance_specification(Start_AST, Start_Errors, Next_AST, Next_Errors),
  gram_occurance_specifications(Next_AST, Next_Errors, Final_AST, Final_Errors).

gram_occurance_specifications(Next_AST, Next_Errors, Next_AST, Next_Errors) --> [].

%
% occurance specifications have a few special cases, * + ?
% They are otherwise specified with {Min,Max} or {Count} notation
% TODO, we could clean this up a bit
%
gram_occurance_specification(Sub_AST, Sub_Errors, ast_occurance(Sub_AST, none, some(1)), Sub_Errors) -->
  [('?', _)].
gram_occurance_specification(Sub_AST, Sub_Errors, ast_occurance(Sub_AST, none, none), Sub_Errors) -->
  [('*', _)].
gram_occurance_specification(Sub_AST, Sub_Errors, ast_occurance(Sub_AST, some(1), none), Sub_Errors) -->
  [('+', _)].
gram_occurance_specification(Sub_AST, Sub_Errors, ast_occurance(Sub_AST, Min, Max), Final_Errors) -->
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
gram_matching_symbol(AST, []) -->
  [('d', _)],
  {
    char_code('0', Min_Code),
    char_code('9', Max_Code),
    AST = ast_range(Min_Code, Max_Code)
  }.

gram_matching_symbol(AST, []) -->
  [('D', _)],
  {
    char_code('0', Min_Code),
    char_code('9', Max_Code),
    AST = ast_not(ast_range(Min_Code, Max_Code))
  }.

gram_matching_symbol(AST, []) -->
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
    AST = ast_or(
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

gram_matching_symbol(AST, []) -->
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
    AST = ast_not(
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

gram_matching_symbol(AST, []) -->
  [('s', _)],
  {
    char_code(' ', Space_Code),
    char_code('\t', Tab_Code),
    AST = ast_or(
      ast_range(Space_Code, Space_Code),
      ast_range(Tab_Code, Tab_Code)
    )
  }.

gram_matching_symbol(AST, []) -->
  [('s', _)],
  {
    char_code(' ', Space_Code),
    char_code('\t', Tab_Code),
    AST = ast_not(
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
gram_special_symbol(AST, Errors) -->
  [('\\', _)],
  (gram_control_symbol(AST, Errors) ;
   gram_operator_symbol(AST, Errors) ;
   gram_matching_symbol(AST, Errors)).

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
gram_class_non_range_symbol(AST, Errors) -->
  [('\\', _)], !, (gram_control_symbol(AST, Errors) ;
   gram_matching_symbol(AST, Errors)).

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

gram_class_definition(AST, Errors) -->
  [('[', Start_Pos)],
  maybe_not_class_definition(Start_Pos, AST, Errors).

maybe_not_class_definition(Start_Pos, ast_not(AST), Errors) -->
  [('^', _)],
  middle_of_class_definition(Start_Pos, AST, Errors).

maybe_not_class_definition(Start_Pos, AST, Errors) -->
  middle_of_class_definition(Start_Pos, AST, Errors).

middle_of_class_definition(Start_Pos, AST, Errors) -->
  gram_class_members(Start_Pos, AST, Member_Errors),
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

gram_class_member(AST, Errors) -->
  gram_class_non_range_symbol(AST, Errors).

maybe_class_members(First_AST, First_Errors, AST, Errors) -->
  gram_class_member(New_AST, New_Errors),
  {
    Next_AST = ast_or(First_AST, New_AST),
    append(First_Errors, New_Errors, Next_Errors)
  },
  maybe_class_members(Next_AST, Next_Errors, AST, Errors).

maybe_class_members(AST, Errors, AST, Errors) --> [].

gram_class_members(_Start_Pos, AST, Errors) -->
  gram_class_member(First_AST, First_Errors),
  maybe_class_members(First_AST, First_Errors, AST, Errors).

gram_class_members(Start_Pos, ast_error, Errors) -->
  [],
  {
    Errors = [some("No members in class defintions", some(Start_Pos))]
  }.

gram_symbol(AST, Errors) -->
  [('\\', _)],
  (gram_matching_symbol(AST, Errors) ; gram_control_symbol(AST, Errors) ; gram_operator_symbol(AST, Errors)).

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

%gram_single(AST, Errors) --> gram_range_section(AST, Errors).
gram_single(AST, Errors) --> gram_class_definition(AST, Errors).
gram_single(ast_wildcard, []) --> [ ('.', _) ].
gram_single(AST_Node, Errors) --> [('(', _)], gram_expr(AST_Node, Errors), [(')', _)].
gram_single(AST_Node, All_Errors) -->
  [('(', Pos)],
  gram_expr(AST_Node, Errors), ! ,
  {
    append(Errors, [error("No closing parenthesis", some(Pos))], All_Errors)
  }.
gram_single(ast_error, All_Errors) -->
  [('(', Pos)], !,
  {
    All_Errors = [error("No closing parenthesis", some(Pos))]
  }.
gram_single(AST, Errors) --> gram_symbol(AST, Errors).

%! ast_to_dot(+AST, +Stream) is det.
%
%   Write the dot representation of the AST to the specified stream.
%
%   @arg AST The AST to write out
%   @arg Stream The stream to write the dot represenation too
ast_to_dot(AST, Stream) :-
  format(Stream, "digraph AST {~n", []),
  ast_to_dot_r(Stream, AST, 0, _),
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

ast_to_dot_r(Stream, ast_not(Sub_AST), Current_Index, Next_Index) :-
  Sub_AST_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_AST, Sub_AST_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: not\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_Index]).

ast_to_dot_r(Stream, ast_occurance(Sub_AST, Min, Max), Current_Index, Next_Index) :-
  Sub_AST_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_AST, Sub_AST_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Occurance, ~w - ~w\"];~n", [Current_Index, Current_Index, Min, Max]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_Index]).

ast_to_dot_r(Stream, ast_concat(Sub_AST_L, Sub_AST_R), Current_Index, Next_Index) :-
  Sub_AST_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_AST_L, Sub_AST_L_Index, Sub_AST_R_Index),
  ast_to_dot_r(Stream, Sub_AST_R, Sub_AST_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Concat\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_L_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_R_Index]).

ast_to_dot_r(Stream, ast_or(Sub_AST_L, Sub_AST_R), Current_Index, Next_Index) :-
  Sub_AST_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_AST_L, Sub_AST_L_Index, Sub_AST_R_Index),
  ast_to_dot_r(Stream, Sub_AST_R, Sub_AST_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Or\"];~n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_L_Index]),
  format(Stream, "\t~d -> ~d;~n", [Current_Index, Sub_AST_R_Index]).


%! combined_asts(+ASTs, -Combined_AST) is det.
%! combined_asts(-ASTs, +Combined_AST) is det.
%
%   Combined_AST is ASTs combined with logical OR.
%
%   @arg ASTs The list of ASTs to combine with logical OR
%   @arg Combined_AST The result of combining ASTs with logical OR
combined_asts([First_AST | Rest_Of_ASTs], Combined_AST) :-
  foldl(combined_asts_fold, Rest_Of_ASTs, First_AST, Combined_AST).

combined_asts_fold(Current_AST, Last_AST, Next_AST) :-
  Next_AST = ast_or(Current_AST, Last_AST).

:- begin_tests(regexc_ast).

% A correct string has a 1-1 relationship with some AST
test_correct_string(Correct_String, AST) :-
  bagof(Possible_AST, string_ast(Correct_String, Possible_AST, Errors), ASTs),
  % There is only one AST.
  % TODO: I think maybe there shouldn't be any choice points here?
  % I think pltest has the easy facility to test a deterministic function
  assertion(ASTs = [AST]),

  % There are no errors
  assertion(Errors = []).

% An incorrection sting will have some ast artifact, and a non-emtpy list of errors.
test_incorrect_string(Incorrect_String, AST, Errors) :-
  bagof(
    (Possible_AST, Possible_Errors),
    string_ast(Incorrect_String, Possible_AST, Possible_Errors),
    Outputs
  ),
  % There should still only be one possible interpretation of the input string.
  % I think.
  assertion(Outputs = [(AST, Errors)]).

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
  forall(member((Incorrect_Input, Matching_AST, Matching_Errors), Incorrect_Strings),
    assertion(test_incorrect_string(Incorrect_Input, Matching_AST, Matching_Errors))
  ).

test_combined_ast(ASTs, Correct_AST) :-
  combined_asts(ASTs, AST),
  assertion(AST = Correct_AST).

test(combined_asts) :-
  Combined_ASTs = [
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
  forall(member((ASTs, Correct_AST), Combined_ASTs),
    assertion(test_combined_ast(ASTs, Correct_AST))
  ).

test_dot_output(String, Correct_Dot_File) :-
  string_ast(String, AST, Errors),
  assertion(Errors = []),

  tmp_file_stream(text, Test_File, TO), close(TO),
  write_to_file_once(ast_to_dot(AST), Test_File, _Deterministic),
  %assertion(Deterministic = true),
  file_diff(Test_File, Correct_Dot_File, Diff),

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

:- end_tests(regexc_ast).
