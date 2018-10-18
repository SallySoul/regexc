gram_expr(Ast_Node) --> gram_or(Ast_Node).

gram_or(Ast_Node) --> gram_concat(Ast_Node).
gram_or(ast_or(Ast_Node1, Ast_Node2)) --> gram_concat(Ast_Node1), ['|'], gram_concat(Ast_Node2).

gram_concat(Ast_Node) --> gram_occurance(Ast_Node).
gram_concat(ast_concat(Ast_Node1, Ast_Node2)) --> gram_occurance(Ast_Node1), gram_concat(Ast_Node2).

gram_occurance(Ast_Node) --> gram_single(Ast_Node).
gram_occurance(ast_occurance(Ast_Node, none, none)) --> gram_single(Ast_Node), ['*'].
gram_occurance(ast_occurance(Ast_Node, none, 1)) --> gram_single(Ast_Node), ['?'].
gram_occurance(ast_occurance(Ast_Node, 1, none)) --> gram_single(Ast_Node), ['+'].

gram_single(ast_char(X)) --> [ X ], { char(X) }.
gram_single(ast_wildcard) --> [ '.' ].
gram_single(Ast_Node) --> ['('], gram_expr(Ast_Node), [')'].

string_ast(String, Root_Node) :- 
  % We use the atom-as-char, let swipl deal with specifics
  % So this takes string, relates it to a list of chars
  string_chars(String, Cs), 
 
  % Here we relate the list of chars to the grammer
  phrase(gram_expr(Root_Node), Cs).




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

