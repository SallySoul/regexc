:- module(util,
  [
    enumeration/2
  ]).

/** <module> util

This module contains some helpful predicates that can be shared / don't belong anywhere else

@author Sally Soul
@license MIT
*/

%! enumeration(+List:list, +Enumerated_List:kist) is semidet.
%
% This relates a list to a list of tuples with the element and their index.
enumeration([], []).
enumeration(Ls, Es) :- enumeration_r(Ls, Es, 0).
enumeration_r([], [], _).
enumeration_r([L|Ls], [(L, C)|Es], C) :- N is C + 1, enumeration_r(Ls, Es, N).

:- begin_tests(util).

test_enumeration(Input, Correct_Output) :-
  enumeration(Input, Output),
  assertion(Output = Correct_Output).

test(enumeration) :- 
  Enumerated_Lists = [
    (
      [],
      []
    ),
    (
      ["a"],
      [("a", 0)]
    ),
    (
      [3, 2, 1],
      [(3, 0), (2, 1), (1, 2)]
    ),
    (
      ["a", 0, "b", "c"],
      [("a", 0), (0, 1), ("b", 2), ("c", 3)]
    )
  ],
  forall(member((Input, Correct_Output), Enumerated_Lists),
    assertion(test_enumeration(Input, Correct_Output))
  ).

:- end_tests(util).
