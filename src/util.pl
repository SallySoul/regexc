:- module(util,
  [
    print_errors/2,
    print_error/2,
    enumeration/2
  ]).

/* <module> util

This module contains some helpful predicates that can be shared / don't belong anywhere else

@author Sally Soul
@license MIT
*/

%! print_errors(+Input:string, +Errors:list) is det. 
%
% This predicate prints out the error list in a nicely formated way
%
% @arg Input The original string being parsed.
% @arg Errors The list of errors to print
print_errors(_, []).
print_errors(Input, [Error|Rest_Of_Errors]) :-
  print_error(Input, Error),
  print_errors(Rest_Of_Errors).


%! print_errors(+Input:string, +Error:list) is det. 
%
% This predicate prints out the error in a nicely formated way
%
% @arg Input The original string being parsed.
% @arg Error The list of errors to print
print_error(Input, error(Message, some(Pos))) :-
  format("~s~n~s at ~d", [Input, Message, Pos]).

%! enumeration(+List:list, +Enumerated_List:kist) is semidet.
%
% This relates a list to a list of tuples with the element and their index.
enumeration([], []).
enumeration(Ls, Es) :- enumerate_r(Ls, Es, 0).
enumeration_r([], [], _).
enumeration_r([L|Ls], [(L, C)|Es], C) :- N is C + 1, enumerate_r(Ls, Es, N).
