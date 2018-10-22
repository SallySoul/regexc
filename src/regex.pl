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
  % We use the atom-as-char, let swipl deal with specifics
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

gram_single(ast_char(X), []) --> [ (X, _) ], { char(X) }.
gram_single(ast_wildcard, []) --> [ ('.', _) ].
gram_single(Ast_Node, Errors) --> [('(', _)], gram_expr(Ast_Node, Errors), [(')', _)].
gram_single(Ast_Node, All_Errors) --> 
  [('(', Pos)], 
  gram_expr(Ast_Node, Errors),
  {
    append(Errors, [error("No closing parenthesis"), some(Pos)], All_Errors)
  }.

/*
ast_occurance --> ast_single, ['{'], maybe_int(_),  [','], maybe_int(_), ['}'].

maybe_int(some(I))--> integer(I).
maybe_int(none) --> [].

integer(I) -->
  digit(D0),
  digits(D),
  { number_chars(I, [D0|D]}.
*/

digits([D|T]) -->
  digit(D), !,
  digits(T).
digits([]) -->
  [].

digit(D) -->
  [D],
  { char_type(D, digit)}.

%! char(C).
char('a').
char('b').
char('c').

