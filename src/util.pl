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

write_to_file(Goal, Path) :-
  absolute_file_name(Path, Absolute_Path),
  open(Absolute_Path, write, File_Output),
  call(Goal, File_Output),
  close(File_Output).

file_diff(Path_1, Path_2, Diff) :-
  absolute_file_name(Path_1, Absolute_Path_1),
  absolute_file_name(Path_2, Absolute_Path_2),
  assertion(exists_file(Absolute_Path_1)),
  assertion(exists_file(Absolute_Path_2)),
  process_create(
    "/usr/bin/env",
    ["git", "diff", Absolute_Path_1, Absolute_Path_2],
    [process(Diff_PID), stdout(pipe(Diff_Output))]
  ),
  process_wait(
    Diff_PID,
    Status,
    [timeout(1)]
  ),
  assertion(Status = exit(0)),
  read_string(Diff_Output, _, Diff).

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
