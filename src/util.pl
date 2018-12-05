:- module(util,
  [
    print_errors/2,
    print_error/2,
    enumeration/2
  ]).

/** <module> util

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
print_errors(Input, Errors) :-
  maplist(print_error(Input), Errors).

write_single_arrow(0) :-
  write('^\n'), !.
write_single_arrow(N) :-
  write(' '),
  M is N - 1,
  write_single_arrow(M).

%! print_errors(+Input:string, +Error:list) is det. 
%
% This predicate prints out the error in a nicely formated way
%
% @arg Input The original string being parsed.
% @arg Error The list of errors to print
% TODO: We should probably propagate information about where regex came from
print_error(Input, error(Message, some(Pos))) :-
  format('ERROR: ~s at ~d~n', [Message, Pos]),
  format('~w~n', [Input]),
  write_single_arrow(Pos).

%! enumeration(+List:list, +Enumerated_List:kist) is semidet.
%
% This relates a list to a list of tuples with the element and their index.
enumeration([], []).
enumeration(Ls, Es) :- enumeration_r(Ls, Es, 0).
enumeration_r([], [], _).
enumeration_r([L|Ls], [(L, C)|Es], C) :- N is C + 1, enumeration_r(Ls, Es, N).

:- begin_tests(util).

test(enumeration_1) :- 
  enumeration([3, 2, 1], [(3, 0), (2, 1), (1, 2)]).

:- end_tests(util).
