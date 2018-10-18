enumeration([], []).
enumeration(Ls, Es) :- enumerate_r(Ls, Es, 0).
enumerate_r([], [], _).
enumerate_r([L|Ls], [(L, C)|Es], C) :- N is C + 1, enumerate_r(Ls, Es, N).

gram_expr(Ast_Node, Errors) --> gram_or(Ast_Node, Errors).

gram_or(Ast_Node, Errors) --> gram_concat(Ast_Node, Errors).
gram_or(ast_or(Ast_Node1, Ast_Node2), All_Errors) --> 
  gram_concat(Ast_Node1, Errors1), 
  ['|'], 
  gram_concat(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.

gram_concat(Ast_Node, Errors) --> gram_occurance(Ast_Node, Errors).
gram_concat(ast_concat(Ast_Node1, Ast_Node2), All_Errors) --> 
  gram_occurance(Ast_Node1, Errors1), 
  gram_concat(Ast_Node2, Errors2),
  { append(Errors1, Errors2, All_Errors)}.

gram_occurance(Ast_Node, Errors) --> gram_single(Ast_Node, Errors).
gram_occurance(ast_occurance(Ast_Node, none, none), Errors) --> gram_single(Ast_Node, Errors), ['*'].
gram_occurance(ast_occurance(Ast_Node, none, 1), Errors) --> gram_single(Ast_Node, Errors), ['?'].
gram_occurance(ast_occurance(Ast_Node, 1, none), Errors) --> gram_single(Ast_Node, Errors), ['+'].

gram_single(ast_char(X), []) --> [ X ], { char(X) }.
gram_single(ast_wildcard, []) --> [ '.' ].
gram_single(Ast_Node, Errors) --> ['('], gram_expr(Ast_Node, Errors), [')'].
gram_single(Ast_Node, All_Errors) --> 
  ['('], 
  gram_expr(Ast_Node, Errors),
  {append(Errors, [error("Expected ')'")], All_Errors)}.

string_ast(String, Root_Node, Errors) :- 
  % We use the atom-as-char, let swipl deal with specifics
  % So this takes string, relates it to a list of chars
  string_chars(String, Cs), 
 
  % Here we relate the list of chars to the grammer
  phrase(gram_expr(Root_Node, Errors), Cs).



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

char('a').
char('b').
char('c').

