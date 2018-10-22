:- module(tree,
  [
    name_var/2
  ]).

name_var(ast_or(Ast_Node1, Ast_Node2), named_or(Name, Named_Node1, Named_Node2)) :-
  name_var(Ast_Node1, Named_Node1),
  name_var(Ast_Node2, Named_Node2).

name_var(ast_concat(Ast_Node1, Ast_Node2), named_concat(Name, Named_Node1, Named_Node2)) :-
  name_var(Ast_Node1, Named_Node1),
  name_var(Ast_Node2, Named_Node2).

name_var(ast_occurance(Ast_Note, Min, Max), named_occurance(Name, Named_Node, Min, Max)) :-
  name_var(Ast_Node, Named_Node).

name_var(ast_char(X), named_char(Name, X)).
name_var(ast_wildcard(X), named_char(Name, X)).

