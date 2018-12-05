:- module(regex,
  [
    string_ast/3
  ]).

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
string_ast(String, Root_Node, Errors) :- 
  % We /* use  */the atom-as-char, let swipl deal with specifics
  % So this takes string, relates it to a list of chars / their positon
  string_chars(String, Chars), 
  util:enumeration(Chars, Enumerated_Chars),
 
  % Here we relate the list of chars to the grammer
  phrase(gram_expr(Root_Node, Errors), Enumerated_Chars).

gram_expr(Ast_Node, Errors) --> gram_or(Ast_Node, Errors).

gram_or(Ast_Node, Errors) --> gram_concat(Ast_Node, Errors).
gram_or(ast_or(Ast_Node1, Ast_Node2), All_Errors) --> 
  gram_concat(Ast_Node1, Errors1), 
  [('|', _)], 
  gram_concat(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.

gram_concat(Ast_Node, Errors) --> gram_occurance(Ast_Node, Errors).
gram_concat(ast_concat(Ast_Node1, Ast_Node2), All_Errors) --> 
  gram_occurance(Ast_Node1, Errors1), 
  gram_concat(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.


gram_occurance(Ast_Node, Errors) --> gram_single(Ast_Node, Errors).
gram_occurance(ast_occurance(Ast_Node, none, none), Errors) --> gram_single(Ast_Node, Errors), [('*', _)].
gram_occurance(ast_occurance(Ast_Node, none, 1), Errors) --> gram_single(Ast_Node, Errors), [('?', _)].
gram_occurance(ast_occurance(Ast_Node, 1, none), Errors) --> gram_single(Ast_Node, Errors), [('+', _)].
% TODO define how to parse Expr{3, 5}

gram_single(ast_char(X), []) --> [ (X, _) ], { char(X) }.
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

ast_to_dot(Stream, Ast) :-
  writeln(Stream, "digraph AST {"),
  ast_to_dot_r(Stream, Ast, 0, _),
  writeln(Stream, "}").

ast_to_dot_r(Stream, ast_wildcard, Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1,
  format(Stream, "\t~d [label=\"~d: wildcard\"]\n", [Current_Index, Current_Index]).

ast_to_dot_r(Stream, ast_char(C), Current_Index, Next_Index) :-
  Next_Index is Current_Index + 1, 
  format(Stream, "\t~d [label=\"~d: char(~a)\"]\n", [Current_Index, Current_Index, C]).

ast_to_dot_r(Stream, ast_occurance(Sub_Ast, Min, Max), Current_Index, Next_Index) :-
  Sub_Ast_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast, Sub_Ast_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Occurance, ~w - ~w]\"\n", [Current_Index, Current_Index, Min, Max]),
  format(Stream, "\t~d -> ~d\n", [Current_Index, Sub_Ast_Index]).

ast_to_dot_r(Stream, ast_concat(Sub_Ast_L, Sub_Ast_R), Current_Index, Next_Index) :-
  Sub_Ast_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast_L, Sub_Ast_L_Index, Sub_Ast_R_Index),
  ast_to_dot_r(Stream, Sub_Ast_R, Sub_Ast_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Concat]\"\n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d\n", [Current_Index, Sub_Ast_L_Index]),
  format(Stream, "\t~d -> ~d\n", [Current_Index, Sub_Ast_R_Index]).

ast_to_dot_r(Stream, ast_or(Sub_Ast_L, Sub_Ast_R), Current_Index, Next_Index) :-
  Sub_Ast_L_Index is Current_Index + 1,
  ast_to_dot_r(Stream, Sub_Ast_L, Sub_Ast_L_Index, Sub_Ast_R_Index),
  ast_to_dot_r(Stream, Sub_Ast_R, Sub_Ast_R_Index, Next_Index),
  format(Stream, "\t~d [label=\"~d: Or]\"\n", [Current_Index, Current_Index]),
  format(Stream, "\t~d -> ~d\n", [Current_Index, Sub_Ast_L_Index]),
  format(Stream, "\t~d -> ~d\n", [Current_Index, Sub_Ast_R_Index]).

/*
ast_occurance --> ast_single, ['{'], maybe_int(_),  [','], maybe_int(_), ['}'].

maybe_int(some(I))--> integer(I).
maybe_int(none) --> [].

integer(I) -->
  digit(D0),
  digits(D),
  { number_chars(I, [D0|D]}.
*/
/*
digits([D|T]) -->
  digit(D), !,
  digits(T).
digits([]) -->
  [].
*/
digit(D) -->
  [D],
  { char_type(D, digit)}.

%! char(C).
char('a').
char('b').
char('c').

:- begin_tests(regex).

% A correct string has a 1-1 relationship with some Ast
test_correct_string(Correct_String, Ast) :- 
  % There are no errors
  Errors = [],
  bagof(Possible_Ast, string_ast(Correct_String, Possible_Ast, Errors), Asts),
  % There is only one Ast. 
  % TODO: I think maybe there shouldn't be any choice points here?
  % I think pltest has the easy facility to test a deterministic function
  Asts = [Ast].

% An incorrection sting will have some ast artifact, and a non-emtpy list of errors.
test_incorrect_string(Incorrect_String, Ast, Errors) :-
  bagof(
    (Possible_Ast, Possible_Errors), 
    string_ast(Incorrect_String, Possible_Ast, Possible_Errors),
    Outputs
  ),
  % There should still only be one possible interpretation of the input string.
  % I think.
  Outputs = [(Ast, Errors)].

test(correct_strings) :- 
  Correct_Strings= [
    (
      "a", 
      ast_char(a)),
    (
      "b", 
      ast_char(b)),
    (
      ".", 
      ast_wildcard
    ),
    (
      "a|b", 
      ast_or(ast_char(a), ast_char(b))
    ),
    (
      "a*", 
      ast_occurance(ast_char(a), none, none)
    ),
    (
      "a?", 
      ast_occurance(ast_char(a), none, 1)
    ),
    (
      "a+", 
      ast_occurance(ast_char(a), 1, none)
    ),
    (
      "ab", 
      ast_concat(ast_char(a), ast_char(b))
    ),
    (
      "ab|bc", 
      ast_or(ast_concat(ast_char(a), ast_char(b)), ast_concat(ast_char(b), ast_char(c)))
    ),
    (
      "(ab)*", 
      ast_occurance(ast_concat(ast_char(a), ast_char(b)), none, none)
    )
  ],
  forall(member((Correct_Input, Correct_Output), Correct_Strings),
         assertion(test_correct_string(Correct_Input, Correct_Output))).

  test(incorrect_strings) :- 
    Incorrect_Strings = [
      (
        "(",
        ast_error,
        [error("No closing parenthesis", some(0))]
      ),
      (
        "(a",
        ast_char(a),
        [error("No closing parenthesis", some(0))]
      )
  ],
  forall(member((Incorrect_Input, Matching_Ast, Matching_Errors), Incorrect_Strings),
         assertion(test_incorrect_string(Incorrect_Input, Matching_Ast, Matching_Errors))).

:- end_tests(regex).
